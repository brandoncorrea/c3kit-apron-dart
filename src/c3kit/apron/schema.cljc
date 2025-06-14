(ns c3kit.apron.schema
  "Defines data structure, coerces, validates."
  (:refer-clojure :exclude [uri?])
  (:require [c3kit.apron.corec :as ccc]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            #?(:cljs [com.cognitect.transit.types])         ;; https://github.com/cognitect/transit-cljs/issues/41
            #?(:cljs [cljs-time.core])))                    ; needed for time

(comment
  "Schema Sample"
  {:field
   {:type        :string                                    ;; see type-validators for list
    :db          [:unique-value]                            ;; passed to database
    :coerce      [#(str % "y")]                             ;; single/list of coerce fns
    :validate    [#(> (count %) 1)]                         ;; single/list of validation fns
    :message     "message describing the field"             ;; coerce failure message (or :validate failure message)
    :validations [{:validate fn :message "msg"}]            ;; multiple validation/message pairs
    :present     [#(str %)]}})                              ;; single/list of presentation fns

(comment
  ;; Shorthands
  {:type [:int] :validate even?} {:type :seq :spec {:type :int :validate even?}}
  {:type [{:type :int}] :validate seq} {:type :seq :spec {:type :int} :validate seq}
  {:type {:foo {}}} {:type :map :schema {:foo {}}}
  {:type #{:string :int}} {:type :one-of :specs [{:type :string} {:type :int}]})

;; TODO - MDM: [{:type :long}] - seq fields should contain a spec.  Processes on the seq field should act on the
;;   seq value, not the values in the seq.  The spec will specify what processes act on the values in the seq.
;; TODO - MDM: Deprecation print message on deprecated fns

(defn- coerce-ex [value type]
  (let [value-str (pr-str value)
        value-str (if (< 50 (count value-str))
                    (str (subs value-str 0 45) "...")
                    value-str)]
    (ex-info (str "can't coerce " value-str " to " type) {:value value :type type})))

(defn- validation-ex [message value]
  (ex-info (or message "is invalid") {:value value}))

(def date #?(:cljd dart:core/DateTime
             :clj java.util.Date
             :cljs js/Date))

;; region ----- Common Validations -----

(defn present? [v]
  (not (or (nil? v)
           (and (string? v) (str/blank? v)))))

(def required {:validate present? :message "is required"})

(defn nil?-or [f] (some-fn nil? f))

(def email-pattern #"[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?\.)+[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?")

(defn email? [value] (boolean (re-matches email-pattern value)))

(defn bigdec? [v]
  #?(:clj  (instance? BigDecimal v)
     :cljs (number? v)
     :cljd (double? v)))

(defn uri? [value]
  #?(:clj  (instance? java.net.URI value)
     :cljd (instance? dart:core/Uri value)
     :cljs (string? value)))

(defn is-enum? [enum]
  (let [enum-name (name (:enum enum))
        enum-set  (ccc/map-set #(keyword enum-name (name %)) (:values enum))]
    (fn [value]
      (or (nil? value)
          (contains? enum-set value)))))

;; endregion ^^^^^ Common Validations ^^^^^
;; region ----- Common Coercions -----

#?(:cljs
   (defn parse! [f v]
     (let [result (f v)]
       (if (js/isNaN result)
         (throw (js/Error "parsed NaN"))
         result))))

(defn ->boolean [value]
  (cond (nil? value) nil
        (boolean? value) value
        (string? value) (not= "false" (str/lower-case value))
        :else (boolean value)))

(defn ->string [value] (some-> value str))
(defn str-or-nil [v] (->string v))

(defn ->keyword [value]
  (cond
    (nil? value) nil
    (keyword? value) value
    :else (let [s (str value)]
            (if (str/starts-with? s ":")
              (keyword (subs s 1))
              (keyword s)))))

(defn ->float [v]
  (cond
    (nil? v) nil
    (string? v) (when-not (str/blank? v)
                  (try
                    #?(:cljd (dart:core/double.parse v)
                       :clj  (Double/parseDouble v)
                       :cljs (parse! js/parseFloat v))
                    (catch #?(:clj Exception :cljd Object :cljs :default) _
                      (throw (coerce-ex v "float")))))
    #?@(:clj [(char? v) (-> v str ->float)])
    #?@(:cljs [(js/isNaN v) nil])
    (integer? v) (double v)
    (#?(:cljd double? :clj float? :cljs number?) v) v
    #?@(:clj [(bigdec? v) (.doubleValue v)])
    #?@(:cljs [(bigdec? v) v])
    :else (throw (coerce-ex v "float"))))

(defn ->int [v]
  (cond
    (nil? v) nil
    (string? v) (when-not (str/blank? v)
                  (try
                    #?(:cljd (.toInt (dart:core/double.parse v))
                       :clj  (long (Double/parseDouble v))
                       :cljs (parse! js/parseInt v))
                    (catch #?(:clj Exception :cljd Object :cljs :default) _
                      (throw (coerce-ex v "int")))))
    (keyword? v) (throw (coerce-ex v "int"))
    #?@(:clj [(char? v) (-> v str ->int)])
    #?@(:cljs [(js/isNaN v) nil])
    (integer? v) v
    (#?(:cljd double? :clj float? :cljs number?) v) (long v)
    #?@(:clj [(bigdec? v) (.intValue v)])
    #?@(:cljs [(bigdec? v) v])
    :else (throw (coerce-ex v "int"))))

(defn ->bigdec [v]
  (cond
    (nil? v) nil
    (string? v) (when-not (str/blank? v)
                  (try
                    #?(:cljd (dart:core/double.parse v)
                       :clj  (bigdec v)
                       :cljs (parse! js/parseFloat v))
                    (catch #?(:clj Exception :cljd Object :cljs :default) _
                      (throw (coerce-ex v "bigdec")))))
    #?@(:clj [(char? v) (-> v str ->bigdec)])
    #?@(:cljs [(js/isNaN v) nil])
    (integer? v) #?(:cljd (double v) :clj (bigdec v) :cljs (double v))
    (#?(:cljd double? :clj float? :cljs number?) v) #?(:cljd v :clj (bigdec v) :cljs v)
    #?@(:clj [(bigdec? v) v])
    :else (throw (coerce-ex v "bigdec"))))

(defn- date? [v]
  (instance?
    #?(:cljd dart:core/DateTime
       :clj  java.util.Date
       :cljs js/Date)
    v))

(defn- date-from-millis [millis-since-epoch]
  #?(:cljd (dart:core/DateTime.fromMillisecondsSinceEpoch millis-since-epoch .isUtc true)
     :clj  (doto (java.util.Date.) (.setTime millis-since-epoch))
     :cljs (doto (js/Date.) (.setTime millis-since-epoch))))

(defn ->date [v]
  (cond
    (nil? v) nil
    (date? v) v
    (integer? v) (date-from-millis v)
    #?@(:cljs [(instance? goog.date.Date v) (js/Date. (.getTime v))])
    (string? v) (cond
                  (str/blank? v) nil
                  (str/starts-with? v "#inst") (edn/read-string v)
                  :else (throw (coerce-ex v "date")))
    :else (throw (coerce-ex v "date"))))

(defn ->sql-date [v]
  (cond
    (nil? v) nil
    #?(:clj (instance? java.sql.Date v) :default (date? v)) v
    #?@(:clj [(instance? java.util.Date v) (java.sql.Date. (.getTime v))])
    (integer? v) #?(:clj (java.sql.Date. v) :default (date-from-millis v))
    #?@(:cljs [(instance? goog.date.Date v) (js/Date. (.getTime v))])
    (string? v) (cond
                  (str/blank? v) nil
                  (str/starts-with? v "#inst") #?(:clj     (java.sql.Date. (.getTime (edn/read-string v)))
                                                  :default (edn/read-string v))
                  :else (throw (coerce-ex v "sql-date")))
    :else (throw (coerce-ex v "sql-date"))))

(defn ->timestamp [v]
  (cond
    (nil? v) nil
    #?(:clj (instance? java.sql.Timestamp v) :default (date? v)) v
    #?@(:clj [(instance? java.util.Date v) (java.sql.Timestamp. (.getTime v))])
    (integer? v) #?(:clj (java.sql.Timestamp. v) :default (date-from-millis v))
    #?@(:cljs [(instance? goog.date.Date v) (js/Date. (.getTime v))])
    (string? v) (cond
                  (str/blank? v) nil
                  (str/starts-with? v "#inst") #?(:clj (java.sql.Timestamp. (.getTime (edn/read-string v))) :default (edn/read-string v))
                  :else (throw (coerce-ex v "timestamp")))
    :else (throw (coerce-ex v "timestamp"))))

(defn ->uri [v]
  (cond
    (nil? v) nil
    #?@(:clj [(instance? java.net.URI v) v])
    #?@(:cljd [(instance? dart:core/Uri v) v])
    (string? v) #?(:cljd (dart:core/Uri.parse v)
                   :clj  (java.net.URI/create v)
                   :cljs v)
    :else (throw (coerce-ex v "uri"))))

(defn- ->map [m]
  (cond (nil? m) m
        (map? m) m
        (sequential? m) (into {} m)
        :else (throw (coerce-ex m "map"))))

;; MDM : https://github.com/cognitect/transit-cljs/issues/41
#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID))

(defn ->uuid [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (string? v) #?(:clj (java.util.UUID/fromString v) :default (uuid v))
    :else (throw (coerce-ex v "uuid"))))

(defn- multiple? [thing]
  (or (sequential? thing)
      (set? thing)))

(defn- ->vec [v]
  (cond
    (nil? v) []
    (multiple? v) (vec v)
    :else [v]))

(defn ->seq [v]
  (cond
    (nil? v) []
    (multiple? v) v
    :else (list v)))

;; endregion ^^^^^ Common Coercions ^^^^^
;; region ----- Type Tables -----

(def type-validators
  {:any       (constantly true)
   :bigdec    (nil?-or bigdec?)
   :boolean   (nil?-or boolean?)
   :date      (nil?-or #(instance? #?(:clj java.sql.Date :cljs date :cljd dart:core/DateTime) %))
   :double    (nil?-or #?(:cljs number? :default float?))
   :float     (nil?-or #?(:cljs number? :default float?))
   :fn        (nil?-or ifn?)
   :ignore    (constantly true)
   :instant   (nil?-or #(instance? #?(:cljd dart:core/DateTime :default date) %))
   :int       (nil?-or integer?)
   :keyword   (nil?-or keyword?)
   :kw-ref    (nil?-or keyword?)
   :long      (nil?-or integer?)
   :map       (nil?-or map?)
   :ref       (nil?-or integer?)
   :seq       (nil?-or multiple?)
   :string    (nil?-or string?)
   :timestamp (nil?-or #(instance? #?(:clj java.sql.Timestamp :cljs date :cljd dart:core/DateTime) %))
   :uri       (nil?-or uri?)
   :uuid      (nil?-or uuid?)})

(def type-coercers
  {:any       identity
   :bigdec    ->bigdec
   :boolean   ->boolean
   :date      ->sql-date
   :double    ->float
   :float     ->float
   :fn        identity
   :ignore    identity
   :instant   ->date
   :int       ->int
   :keyword   ->keyword
   :kw-ref    ->keyword
   :long      ->int
   :map       ->map
   :ref       ->int
   :seq       identity
   :string    ->string
   :timestamp ->timestamp
   :uri       ->uri
   :uuid      ->uuid})

(def valid-types (set (concat (keys type-coercers) [:one-of])))

;; endregion ^^^^^ Type Tables ^^^^^

;; region ----- shorthands -----

(declare normalize-spec)

(defn- normalize-seq-shorthand [{:keys [type] :as spec}]
  (when (not= 1 (count type)) (throw (ex-info "seq shorthand type must contain 1 type" spec)))
  (let [spec-type      (first type)
        base-spec-keys #{:type :validate :coerce :present :message :validations}]
    (cond (keyword? spec-type) (let [entry-spec (assoc (select-keys spec base-spec-keys) :type spec-type)]
                                 (-> (apply dissoc spec base-spec-keys)
                                     (assoc :type :seq :spec entry-spec)))
          (and (map? spec-type) (contains? spec-type :type)) (assoc spec :type :seq :spec (normalize-spec spec-type))
          :else (let [entry-spec (normalize-spec (assoc (select-keys spec base-spec-keys) :type spec-type))]
                  (-> (apply dissoc spec base-spec-keys)
                      (assoc :type :seq :spec entry-spec))))))

(defn- normalize-map-shorthand [{:keys [type] :as spec}]
  (assoc spec :type :map :schema type))

(defn- normalize-set-shorthand [{:keys [type] :as spec}]
  (assoc spec :type :one-of :specs (mapv normalize-spec type)))

(defn normalized?
  "Returns true if the schema-or-spec has been normalized, false otherwise."
  [schema-or-spec] (::normalized? (meta schema-or-spec)))

(defn- normal-spec-form? [spec]
  (and (map? spec) (keyword? (:type spec))))

(defn normalize-spec
  "If the spec is using a shorthand, it will be expanded."
  [spec]
  (if (normalized? spec)
    spec
    (with-meta
      (cond (normal-spec-form? spec) spec
            (keyword? spec) {:type spec}
            (sequential? (:type spec)) (normalize-seq-shorthand spec)
            (map? (:type spec)) (normalize-map-shorthand spec)
            (set? (:type spec)) (normalize-set-shorthand spec)
            :else (throw (ex-info (str "invalid spec: " (pr-str spec)) {:spec spec :invalid-spec true})))
      {::normalized? true})))

(defn normalize-schema
  "Returns the schema with all shorthand specs expanded."
  [schema]
  (if (normalized? schema)
    schema
    (-> (dissoc schema :*)
        (update-vals normalize-spec)
        (merge (select-keys schema [:*]))
        (with-meta {::normalized? true}))))

;; endregion ^^^^^ shorthands ^^^^^

;; region ----- Common Schema Attributes -----

(def omit
  "Used as a :present value to remove the entry from presentation"
  (constantly nil))

(defn kind [key]
  {:type     :keyword
   :value    key
   :validate [#(or (nil? %) (= key %))]
   :coerce   [#(or % key)]
   :message  (str "mismatch; must be " key)})

(def id {:type :ref})

;; endregion ^^^^^ Common Schema Attributes ^^^^^
;; region ----- Processing -----

(defn type-coercer! [type]
  (or (get type-coercers type)
      (throw (ex-info (str "unhandled coercion type: " (pr-str type)) {:coerce? true}))))

(defn type-validator! [type]
  (or (get type-validators type)
      (throw (ex-info (str "unhandled validation type: " (pr-str type)) {}))))

(defprotocol FieldError)

(defrecord CoerceError [message]
  FieldError)

(defrecord ValidateError [message]
  FieldError)

(defrecord ConformError [message]
  FieldError)

(defrecord PresentError [message]
  FieldError)

(defn- -create-field-error [ctor default-message options]
  (let [ex      (:exception options)
        data    (ex-data ex)
        message (or (:message options) (:message data) (ex-message ex) default-message)]
    (with-meta (ctor message) (merge data (dissoc options :message)))))

(defn error-message
  "Return the message of an error."
  [error]
  (:message error))

(defn error-exception
  "Return the exception attached to a FieldError if any, otherwise nil."
  [field-error]
  (-> field-error meta :exception))

(defn error-value
  "Return the value that caused the FieldError, if any, otherwise nil."
  [field-error]
  (-> field-error meta :value))

(defn error-type
  "Return the target type of a CoerceError, if any, otherwise nil."
  [field-error]
  (-> field-error meta :type))

(defn error-data
  "Return the data map associated with the FieldError."
  [field-error]
  (-> field-error meta))

(defn- error->exception [error]
  (ex-info (:message error) {}))

(defn field-error?
  "Returns true if the value is a FieldError, false otherwise."
  [value]
  #?(:clj     (instance? c3kit.apron.schema.FieldError value)
     :default (satisfies? FieldError value)))

(defn error-seq
  "Returns a sequence of all the FieldErrors in a processed entity."
  [entity]
  (cond (field-error? entity) [entity]
        (map? entity) (mapcat error-seq (vals entity))
        (multiple? entity) (mapcat error-seq entity)))

(defn error?
  "Return true if the processed entity has errors, false otherwise."
  [entity]
  ;; MDM: optimizing by storing :error? in metadata on entity did not improve performance noticeably.
  (boolean (seq (error-seq entity))))

(defn -process-error [process options]
  (case process
    :coerce (-create-field-error ->CoerceError "coercion failed" options)
    :validate (-create-field-error ->ValidateError "is invalid" options)
    :conform (-create-field-error ->ConformError "conform failed" options)
    :present (-create-field-error ->PresentError "present failed" options)))

(declare -process-field-spec)
(declare -process-entity-level-spec)

(defn- field-result-or-error [process spec value]
  (try
    (-process-field-spec process spec value)
    (catch #?(:clj Exception :cljd Object :cljs :default) e
      (-process-error process {:exception e}))))

(defn- entity-result-or-error [process key spec entity]
  (try
    (-process-entity-level-spec process key spec entity)
    (catch #?(:clj Exception :cljd Object :cljs :default) e
      (-process-error process {:exception e}))))

(defn- process-validations [validations value]
  (doseq [{:keys [validate message]} validations]
    (let [validate-fns (if (multiple? validate) validate [validate])]
      (doseq [v-fn validate-fns]
        (when-not (v-fn value)
          (throw (validation-ex message value)))))))

(defn- coerce-field-spec [spec value]
  (let [{:keys [type message]} spec
        coerce-fns (conj (->vec (:coerce spec)) (type-coercer! type))]
    (try
      (reduce (fn [result coerce-fn] (coerce-fn result)) value coerce-fns)
      (catch #?(:clj Exception :cljd Object :cljs :default) e
        (-process-error :coerce {:message message :exception e})))))

(defn- validate-field-spec [spec value]
  (let [{:keys [type validate validations message]} spec
        validations (concat [{:validate (type-validator! type) :message message}]
                            (if validate [{:validate validate :message message}] [])
                            validations)]
    (process-validations validations value)
    value))

(defn- conform-field-spec [spec value]
  (let [coerce-result (field-result-or-error :coerce spec value)]
    (if (field-error? coerce-result)
      coerce-result
      (field-result-or-error :validate spec coerce-result))))

(defn- present-field-spec [spec value]
  (let [present-fns (->vec (:present spec))]
    (reduce (fn [result present-fn] (present-fn result)) value present-fns)))

(defn -process-field-spec [process spec value]
  (case process
    :coerce (coerce-field-spec spec value)
    :validate (validate-field-spec spec value)
    :conform (conform-field-spec spec value)
    :present (present-field-spec spec value)))

(defn- coerce-entity-level-spec [key spec entity]
  (let [{:keys [coerce message]} spec
        coerce-fns (->vec coerce)]
    (try
      (let [coerced-entity (reduce (fn [result coerce-fn] (assoc result key (coerce-fn result))) entity coerce-fns)]
        (get coerced-entity key))
      (catch #?(:clj Exception :cljd Object :cljs :default) e
        (-process-error :coerce {:message message :exception e})))))

(defn- validate-entity-level-spec [key spec entity]
  (let [{:keys [validate validations message]} spec
        validations (concat (if validate [{:validate validate :message message}] []) validations)]
    (process-validations validations entity)
    (get entity key)))

(defn- conform-entity-level-spec [key spec entity]
  (let [coerce-result (entity-result-or-error :coerce key spec entity)]
    (if (field-error? coerce-result)
      coerce-result
      (entity-result-or-error :validate key spec (assoc entity key coerce-result)))))

(defn- present-entity-level-spec [key spec entity]
  (let [present-fns      (->vec (:present spec))
        presented-entity (reduce (fn [result present-fn] (assoc result key (present-fn result))) entity present-fns)]
    (get presented-entity key)))

(defn -process-entity-level-spec [process key spec entity]
  (case process
    :coerce (coerce-entity-level-spec key spec entity)
    :validate (validate-entity-level-spec key spec entity)
    :conform (conform-entity-level-spec key spec entity)
    :present (present-entity-level-spec key spec entity)))

(declare process-schema-on-entity)
(declare -process-spec-on-value)

(defn- process-spec-on-seq-entries [process spec value]
  (cond (nil? value) nil
        (not (multiple? value)) (-process-error process {:message (str "[" (:type spec) "] expected")})
        :else (let [result (mapv #(-process-spec-on-value process spec %) value)]
                (if (= :present process)
                  (ccc/removev nil? result)
                  result))))

(defn- process-seq-spec-on-value [process spec value]
  (let [entry-spec (or (:spec spec) {:type :any})]
    (cond (= :coerce process) (let [value (field-result-or-error :coerce spec value)]
                                (if (error? value)
                                  value
                                  (process-spec-on-seq-entries :coerce entry-spec value)))
          (= :conform process) (let [coerced (field-result-or-error :coerce spec value)]
                                 (if (error? coerced)
                                   coerced
                                   (let [conformed (process-spec-on-seq-entries :conform entry-spec coerced)]
                                     (if (error? conformed)
                                       conformed
                                       (field-result-or-error :validate spec conformed)))))
          :else (let [value (process-spec-on-seq-entries process entry-spec value)]
                  (if (error? value)
                    value
                    (field-result-or-error process spec value))))))

(defn process-map-spec-on-value [process spec value]
  (let [schema (:schema spec)]
    (cond (= :coerce process) (let [value (field-result-or-error :coerce spec value)]
                                (if (error? value)
                                  value
                                  (process-schema-on-entity :coerce schema value)))
          (= :conform process) (let [coerced (field-result-or-error :coerce spec value)]
                                 (cond (error? coerced) coerced
                                       (nil? coerced) (field-result-or-error :validate spec coerced)
                                       :else (let [conformed (process-schema-on-entity :conform schema coerced)]
                                               (if (error? conformed)
                                                 conformed
                                                 (field-result-or-error :validate spec conformed)))))
          (map? value) (let [entity (process-schema-on-entity process schema value)]
                         (if (error? entity)
                           entity
                           (field-result-or-error process spec entity)))
          :else (field-result-or-error process spec value))))

(defn process-one-of-on-value [process spec value]
  (let [specs (:specs spec)]
    (if (seq specs)
      (let [results           (mapv #(-process-spec-on-value process % value) specs)
            non-error-results (filter #(not (error? %)) results)]
        (if (seq non-error-results)
          (first non-error-results)
          (-process-error process {:message (or (:message spec) "one-of: no matching spec") :errors results})))
      (-process-error process {:message "one-of: empty specs"}))))

(defn -process-spec-on-value [process spec value]
  (let [spec (normalize-spec spec)
        type (:type spec)]
    (case type
      :seq (process-seq-spec-on-value process spec value)
      :map (process-map-spec-on-value process spec value)
      :one-of (process-one-of-on-value process spec value)
      (field-result-or-error process spec value))))

(defn- process-entity-key-spec [process entity [key spec]]
  (let [value     (get entity key)
        new-value (-process-spec-on-value process spec value)]
    (if (some? new-value)
      (assoc entity key new-value)
      (dissoc entity key))))

(defn- process-entity-level-spec [process result [key spec]]
  (let [value (entity-result-or-error process key spec (:entity result))]
    (if (some? value)
      (if (error? value)
        (assoc-in result [:errors key] value)
        (assoc-in result [:entity key] value))
      (update result :entity dissoc key))))

(defn- process-schema-on-entity [process schema entity]
  (let [entity (select-keys entity (keys schema))
        entity (reduce (partial process-entity-key-spec process) entity (dissoc schema :*))]
    (if (error? entity)
      entity
      (let [{:keys [entity errors]} (reduce (partial process-entity-level-spec process) {:entity entity} (:* schema))]
        (merge entity errors)))))

(defn- attempt-process-schema-on-entity [process schema entity]
  (try
    (process-schema-on-entity process schema (->map entity))
    (catch #?(:clj Exception :cljd Object :cljs :default) e
      (if (:invalid-spec (ex-data e))
        (throw e)
        (-process-error process {:value entity :exception e})))))

(defn- process-value-or-error [process spec value]
  (let [result (-process-spec-on-value process spec value)]
    (if-let [error (first (error-seq result))]
      (throw (error->exception error))
      result)))

;; endregion ^^^^^ Processing ^^^^^

;; region ----- API -----

(defn coerce-value!
  "returns coerced value or throws an exception"
  ([schema key value] (coerce-value! (get schema key) value))
  ([spec value] (process-value-or-error :coerce spec value)))

(defn validate-value!
  "throws an exception when validation fails, value otherwise"
  ([schema key value] (validate-value! (get schema key) value))
  ([spec value] (process-value-or-error :validate spec value)))

(defn valid-value?
  "return true or false"
  ([schema key value] (valid-value? (get schema key) value))
  ([spec value] (try (validate-value! spec value) true (catch #?(:clj Exception :cljd Object :cljs :default) _ false))))

(defn conform-value!
  "coerce and validate, returns coerced value or throws"
  ([schema key value] (conform-value! (get schema key) value))
  ([spec value] (process-value-or-error :conform spec value)))

(defn present-value!
  "returns a presentable representation of the value, or throws"
  ([schema key value] (present-value! (get schema key) value))
  ([spec value] (process-value-or-error :present spec value)))

(defn coerce
  "Returns coerced entity or SchemaError if any coercion failed. Use error? to check result.
  Use Case: 'I want to change my data into the types specified by the schema.'"
  [schema entity]
  (attempt-process-schema-on-entity :coerce schema entity))

(defn validate
  "Returns entity with all values true, or SchemaError when one or more invalid fields. Use error? to check result.
  Use Case: 'I want to make sure all the data is valid according to the schema.'"
  [schema entity]
  (attempt-process-schema-on-entity :validate schema entity))

(defn conform
  "Returns coerced entity or SchemaError upon any coercion or validation failure. Use error? to check result.
  Use Case: 'I want to coerce my data then validate the coerced data, all according to the schema.'
  Use Case: Data comes in from a web-form so strings have to be coerced into numbers, etc., then
            we need to validate that the data is good."
  [schema entity]
  (attempt-process-schema-on-entity :conform schema entity))

(defn present
  "Returns presented entity with FieldErrors where the process failed. Use error? to check result."
  [schema entity]
  (attempt-process-schema-on-entity :present schema entity))

(defn- as-map-or-nil [thing]
  (some->> (seq thing) (into {})))

(defn error-map [result]
  (cond (field-error? result) result
        (map? result) (->> (keep (fn [[k v]] (when-let [v (error-map v)] [k v])) result)
                           as-map-or-nil)
        (multiple? result) (->> (keep-indexed (fn [k v] (when-let [v (error-map v)] [k v])) result)
                                as-map-or-nil)))

(defn message-map
  "nil when there are no errors, otherwise a map {<field> <error message>}."
  ([result]
   (when (error? result)
     (when-let [errors (error-map result)]
       (walk/postwalk (fn [v] (if (field-error? v) (error-message v) v)) errors)))))

(defn message-seq
  "seq of 'friendly' error messages; nil if none."
  [result]
  (when-let [errors (message-map result)]
    (loop [errors errors stack () path () result []]
      (if (empty? errors)
        (if (empty? stack)
          result
          (recur (first stack) (rest stack) (rest path) result))
        (let [[k v] (first errors)
              new-path (cons (if (int? k) (str k) (name k)) path)]
          (if (map? v)
            (recur v (conj stack (rest errors)) new-path result)
            (let [value (str (str/join "." (reverse new-path)) " " v)]
              (recur (rest errors) stack path (conj result value)))))))))

(defn coerce-message-map
  "Runs coerce on the entity and returns a map of error message, or nil if none."
  [schema entity]
  (message-map (coerce schema entity)))

(defn validate-message-map
  "Runs validate on the entity and returns a map of error message, or nil if none."
  [schema entity]
  (message-map (validate schema entity)))

(defn conform-message-map
  "Runs conform on the entity and returns a map of error message, or nil if none."
  [schema entity]
  (message-map (conform schema entity)))

(defn coerce!
  "Returns a coerced entity or throws an exception if there are errors."
  [schema entity]
  (let [result (coerce schema entity)]
    (if (error? result)
      (throw (ex-info "Uncoercable entity" result))
      result)))

(defn validate!
  "Returns a validated entity or throws an exception if there are errors."
  [schema entity]
  (let [result (validate schema entity)]
    (if (error? result)
      (throw (ex-info "Invalid entity" result))
      result)))

(defn conform!
  "Returns a conformed entity or throws an exception if there are errors."
  [schema entity]
  (let [result (conform schema entity)]
    (if (error? result)
      (throw (ex-info "Unconformable entity" result))
      result)))

(defn present!
  "Returns a presented entity or throws an exception if there are errors."
  [schema entity]
  (let [result (present schema entity)]
    (if (error? result)
      (throw (ex-info "Unpresentable entity" result))
      result)))

;; endregion ^^^^^ API ^^^^^

;; region ----- Merging Schemas -----

(defn- validate->validations [{:keys [validate message] :as spec}]
  (if validate
    (-> (dissoc spec :validate)
        (update :validations ccc/conjv {:validate validate :message message}))
    spec))

(defn merge-specs [a b]
  (let [a (validate->validations a)
        b (validate->validations b)]
    (if-let [validations (seq (concat (:validations a []) (:validations b [])))]
      (assoc (merge a b) :validations (vec validations))
      (merge a b))))

(defn merge-schemas [& schemas]
  (let [entity-specs (apply merge-with merge-specs (map :* schemas))
        attr-specs   (apply merge-with merge-specs (map #(dissoc % :*) schemas))]
    (cond-> attr-specs
            (seq entity-specs)
            (assoc :* entity-specs))))

;; endregion ^^^^^ Merging Schemas ^^^^^

;; region ----- spec schema -----

(def ^:private validate-type {:validate #(contains? valid-types %) :message "must be one of schema/valid-types"})
(def process-spec-schema {:type    :one-of
                          :specs   [{:type :fn} {:type :seq :spec {:type :fn}}]
                          :message "must be an ifn or seq of ifn"})

(def validation-schema
  {:validate {:type    :one-of
              :specs   [{:type :fn :validations [required] :message "must be an ifn"}
                        {:type :seq :spec {:type :fn} :validate seq :message "must not be empty"}]
              :message "must be an ifn or seq of ifn"}
   :message  {:type :string}})

(def -spec-schema
  {:type        {:type :keyword :validations [required validate-type]}
   :validate    process-spec-schema
   :coerce      process-spec-schema
   :present     process-spec-schema
   :message     {:type :string}
   :validations {:type :seq :spec {:type :map :schema validation-schema :message "must be schema/validation-schema"}}})

(def spec-schema
  (assoc -spec-schema
    :spec {:type :map :schema -spec-schema :message "must be schema/spec-schema"}
    :specs {:type :seq :spec {:type :map :schema -spec-schema}}
    :schema {:type :map :message "must be a map"}
    :* {:spec   {:validate #(or (not (:spec %)) (= :seq (:type %))) :message "only used with type :seq"}
        :specs  {:validate #(or (not (:specs %)) (= :one-of (:type %))) :message "only used with type :one-of"}
        :schema {:validate #(or (not (:schema %)) (= :map (:type %))) :message "only used with type :map"}}))

(def entity-spec-schema
  (assoc spec-schema
    :type {:type    :keyword :validate (nil?-or #(contains? valid-types %))
           :message "must be one of schema/valid-types"}))

(defn- conform-preserving-extras! [schema spec]
  (let [extra     (apply dissoc spec (keys schema))
        conformed (conform! schema spec)]
    (merge conformed extra)))

(defn conform-schema!
  "Normalizes, coerces, and validates all the specs in the schema.  Any problems in the schema will throw an exception.
  Any extra fields in the spec (:value, :db, ...) will be preserved."
  [schema]
  (let [schema        (normalize-schema schema)
        field-schema  (update-vals (dissoc schema :*) #(conform-preserving-extras! spec-schema %))
        entity-schema (some-> (:* schema) (update-vals #(conform-preserving-extras! entity-spec-schema %)))]
    (cond-> field-schema
            entity-schema
            (assoc :* entity-schema))))

;; endregion ^^^^^ spec schema ^^^^^
