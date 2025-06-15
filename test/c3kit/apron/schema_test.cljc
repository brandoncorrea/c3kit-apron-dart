(ns c3kit.apron.schema-test
  (:require [clojure.test :refer [deftest testing is]]
            [c3kit.apron.schema :as schema]
            [clojure.string :as str]
            [c3kit.apron.utilc :as utilc]
            [c3kit.apron.time :as time]
            [c3kit.apron.corec :as ccc])
  #?@(:cljd ()
      :clj  ((:import (java.net URI)
                      (java.util UUID)))))

(def pet
  {:kind        (schema/kind :pet)
   :id          schema/id
   :species     {:type     :string
                 :validate [#{"dog" "cat" "snake"}]
                 :message  "must be a pet species"}
   :birthday    {:type    :instant
                 :message "must be a date"}
   :length      {:type    :float
                 :message "must be unit in feet"}
   :teeth       {:type     :int
                 :validate [#(and (<= 0 %) (<= % 999))]
                 :message  "must be between 0 and 999"}
   :name        {:type     :string
                 :db       [:unique-value]
                 :coerce   #(str % "y")
                 :validate #(> (count %) 2)
                 :message  "must be nice and unique name"}
   :owner       {:type     :ref
                 :validate [schema/present?]
                 :message  "must be a valid reference format"}
   :colors      {:type [:string] :message "must be a string"}
   ;:ears        {:type     :seq
   ;              :spec     {:type :keyword :validate #{:pointy :floppy} :message "bad ear type"}
   ;              :validate (schema/nil-or? #(= 2 (count %))) :message "must have 2 types"}
   :uuid        {:type :uuid
                 :db   [:unique-identity]}
   :parent      {:type {:name {:type :string}
                        :age  {:type :int}}}
   :temperament {:type :kw-ref}})

(def temperaments
  {:enum   :temperament
   :values [:wild :domestic]})

(def owner
  {:kind (schema/kind :owner)
   :name {:type :string}
   :pet  {:type pet}})

(def household
  {:kind (schema/kind :household)
   :size {:type :long}
   :pets {:type [pet]}})

(def now (-> (time/now) #?(:cljd (-> time/millis-since-epoch time/from-epoch))))
(def home (#?(:clj URI/create :cljs identity :cljd Uri/parse) "http://apron.co"))
(def a-uuid (#?(:clj UUID/fromString :default uuid) "1f50be30-1373-40b7-acce-5290b0478fbe"))

(def valid-pet {:species  "dog"
                :birthday now
                :length   2.5
                :teeth    24
                :name     "Fluffy"
                :owner    12345
                :color    ["brown" "white"]
                :uuid     a-uuid})
(def invalid-pet {:species  321
                  :birthday "yesterday"
                  :length   "foo"
                  :teeth    1000
                  :name     ""
                  :owner    nil
                  :parent   {:age :foo}})

(defn float= [a b delta]
  (<= (- a delta) b (+ a delta)))

(deftest to-boolean-test
  (is (nil? (schema/->boolean nil)))
  (is (false? (schema/->boolean "false")))
  (is (false? (schema/->boolean "FALSE")))
  (is (true? (schema/->boolean "abc")))
  (is (true? (schema/->boolean 1)))
  (is (true? (schema/->boolean 3.14))))

(deftest to-string-test
  (is (nil? (schema/->string nil)))
  (is (= "abc" (schema/->string "abc")))
  (is (= "1" (schema/->string 1)))
  (is (= "3.14" (schema/->string 3.14))))

(deftest to-keyword-test
  (is (nil? (schema/->keyword nil)))
  (is (= :abc (schema/->keyword "abc")))
  (is (= :abc (schema/->keyword ":abc")))
  (is (= :abc/xyz (schema/->keyword "abc/xyz")))
  (is (= :abc/xyz (schema/->keyword ":abc/xyz")))
  (is (= :1 (schema/->keyword 1)))
  (is (= :3.14 (schema/->keyword 3.14)))
  (is (= :foo (schema/->keyword :foo))))

(deftest to-float-test
  (is (nil? (schema/->float nil)))
  (is (nil? (schema/->float "")))
  (is (nil? (schema/->float "\t")))
  (is (= 1.0 (schema/->float \1)))
  (is (= 1.0 (schema/->float 1)))
  (is (float= 3.14 (schema/->float 3.14) 0.00001))
  (is (float= 3.14 (schema/->float "3.14") 0.00001))
  (is (float= 42.0 (schema/->float "42") 0.00001))
  (is (float= 3.14 (schema/->float 3.14M) 0.00001))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->float \a)))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->float "fooey"))))

(deftest to-int-test
  (is (nil? (schema/->int nil)))
  (is (nil? (schema/->int "")))
  (is (nil? (schema/->int "\t")))
  (is (= 1 (schema/->int \1)))
  (is (= 1 (schema/->int 1)))
  (is (= 3 (schema/->int 3.14)))
  (is (= 3 (schema/->int 3.9)))
  (is (= 42 (schema/->int "42")))
  (is (= 3 (schema/->int "3.14")))
  (is (= 3 (schema/->int 3.14M)))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->int \a)))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->int "fooey")))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->int :foo))))

(deftest to-bigdec-test
  (is (nil? (schema/->bigdec nil)))
  (is (nil? (schema/->bigdec "")))
  (is (nil? (schema/->bigdec "\t")))
  (is (= 1M (schema/->bigdec \1)))
  (is (= 1M (schema/->bigdec 1)))
  (is (= 3.14M (schema/->bigdec 3.14)))
  (is (= 3.9M (schema/->bigdec 3.9)))
  (is (= 42M (schema/->bigdec "42")))
  (is (= 3.14M (schema/->bigdec "3.14")))
  (is (= 3.14M (schema/->bigdec 3.14M)))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->bigdec \a)))
  (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                  :cljd cljd.core/ExceptionInfo
                  :cljs cljs.core/ExceptionInfo) (schema/->bigdec "fooey"))))

(deftest coercion

  (testing "to date"
    (is (nil? (schema/->date nil)))
    (is (nil? (schema/->date " \r\n\t")))
    (is (= now (schema/->date now)))
    (is (= now (schema/->date (time/millis-since-epoch now))))
    (is (instance? #?(:clj java.util.Date :cljs js/Date :cljd DateTime) (schema/->date now)))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo
                    :cljs cljs.core/ExceptionInfo) (schema/->date "now")))
    (is (= now (schema/->date (pr-str now)))))

  (testing "to sql date"
    (is (nil? (schema/->sql-date nil)))
    (is (nil? (schema/->sql-date " \r\n\t")))
    (is (= #?(:clj (java.sql.Date. (time/millis-since-epoch now)) :default now) (schema/->sql-date now)))
    (is (= #?(:clj (java.sql.Date. (time/millis-since-epoch now)) :default now) (schema/->sql-date (time/millis-since-epoch now))))
    (is (instance? #?(:clj java.sql.Date :cljs js/Date :cljd DateTime) (schema/->sql-date now)))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo
                    :cljs cljs.core/ExceptionInfo) (schema/->sql-date "now")))
    (is (= #?(:clj (java.sql.Date. (time/millis-since-epoch now)) :default now) (schema/->sql-date (pr-str now)))))

  (testing "to sql timestamp"
    (is (nil? (schema/->timestamp nil)))
    (is (nil? (schema/->timestamp " \r\n\t")))
    (is (= #?(:clj (java.sql.Timestamp. (time/millis-since-epoch now)) :default now) (schema/->timestamp now)))
    (is (= #?(:clj (java.sql.Timestamp. (time/millis-since-epoch now)) :default now) (schema/->timestamp (time/millis-since-epoch now))))
    (is (instance? #?(:clj java.sql.Timestamp :cljs js/Date :cljd DateTime) (schema/->timestamp now)))
    #?(:clj (is (instance? java.sql.Timestamp (schema/->timestamp (java.sql.Date. (time/millis-since-epoch now))))))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo
                    :cljs cljs.core/ExceptionInfo) (schema/->timestamp "now")))
    (is (= #?(:clj (java.sql.Timestamp. (time/millis-since-epoch now)) :default now) (schema/->timestamp (pr-str now)))))

  (testing "to uri"
    (is (nil? (schema/->uri nil)))
    (is (= home (schema/->uri home)))
    (is (= home (schema/->uri "http://apron.co")))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo
                    :cljs cljs.core/ExceptionInfo) (schema/->uri 123))))

  (testing "to uuid"
    (is (nil? (schema/->uuid nil)))
    (is (= a-uuid (schema/->uuid a-uuid)))
    (is (= a-uuid (schema/->uuid "1f50be30-1373-40b7-acce-5290b0478fbe")))
    (is (= (schema/->uuid "53060bf1-971a-4d18-80fc-92a3112afd6e") (schema/->uuid #uuid "53060bf1-971a-4d18-80fc-92a3112afd6e")))
    (let [uuid2    (ccc/new-uuid)
          str-uuid (str uuid2)]
      (is (= uuid2 (schema/->uuid str-uuid))))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo
                    :cljs cljs.core/ExceptionInfo) (schema/->uuid 123))))

  (testing "to seq"
    (is (= [] (schema/->seq nil)))
    (is (= ["foo"] (schema/->seq "foo")))
    (is (= ["foo"] (schema/->seq ["foo"])))
    (is (= ["foo" "bar"] (schema/->seq ["foo" "bar"]))))

  (deftest coercion-from-spec
    (testing "with missing type"
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"invalid spec: \{\}"
            (schema/coerce-value! {} 123))))

    (testing "of boolean"
      (is (true? (schema/coerce-value! {:type :boolean} 123))))

    (testing "of string"
      (is (= "123" (schema/coerce-value! {:type :string} 123))))

    (testing "of int"
      (is (= 123 (schema/coerce-value! {:type :int} "123.4"))))

    (testing "of ref"
      (is (= 123 (schema/coerce-value! {:type :ref} "123.4"))))

    (testing "of float"
      (is (float= 123.4 (schema/coerce-value! {:type :float} "123.4") 0.0001))
      (is (float= 123.4 (schema/coerce-value! {:type :double} "123.4") 0.0001)))

    (testing "of bigdec"
      (is (= 123.4M (schema/coerce-value! {:type :bigdec} "123.4"))))

    (testing "with custom coercions"
      (let [spec {:type :int :coerce [str/trim reverse #(apply str %)]}]
        (is (= 321 (schema/coerce-value! spec " 123\t")))))

    (testing "of schema"
      (let [spec  {:type {:name {:type :string :coerce str/trim}}}
            value {:name "  fred "}]
        (is (= {:name "fred"} (schema/coerce-value! spec value)))))

    (testing "of multi schema"
      (let [spec  {:type [{:name {:type :string :coerce str/trim}}]}
            value {:name "  fred "}]
        (is (= [{:name "fred"}] (schema/coerce-value! spec [value])))))

    (testing "of object with custom coercions"
      (let [spec  {:type   {:name {:type :string}}
                   :coerce (constantly {:name "billy"})}
            value "blah"]
        (is (= {:name "billy"} (schema/coerce-value! spec value)))))

    (testing "of object with nested coercions"
      (let [spec {:type   {:name {:type :string :coerce str/trim}}
                  :coerce (constantly {:name "  billy "})}]
        (is (= {:name "billy"} (schema/coerce-value! spec "blah")))))

    (testing ", custom coercions happen before type coercion"
      (let [spec {:type :string :coerce #(* % %)}]
        (is (= "16" (schema/coerce-value! spec 4)))))

    (testing "of seq"
      (let [result (schema/coerce-value! {:type [:float]} ["123.4" 321 3.1415])]
        (is (float= 123.4 (first result) 0.0001))
        (is (float= 321.0 (second result) 0.0001))
        (is (float= 3.1415 (last result) 0.0001))))

    (testing "of seq from a set"
      (let [result (schema/coerce-value! {:type [:long]} #{"123" 321 3.14})]
        (is (some #{123} result))
        (is (some #{321} result))
        (is (some #{3} result))))

    (testing "of seq with missing spec"
      (let [result (schema/coerce-value! {:type :seq} [1 "2"])]
        (is (= 1 (first result)))
        (is (= "2" (last result)))))

    (testing "of seq with inner coercion"
      (let [result (schema/coerce-value! {:type :seq :spec {:type :float :coerce inc}} [321 3.1415])]
        (is (float= 322.0 (first result) 0.0001))
        (is (float= 4.1415 (last result) 0.0001))))

    (testing "missing multiple type coercer"
      (is (nil? (schema/coerce-value! {:type [:blah]} nil)))
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"\[:long\] expected"
            (schema/coerce-value! {:type [:long]} :foo)))
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"unhandled coercion type: :blah"
            (schema/coerce-value! {:type [:blah]} ["foo"]))))

    (testing "of seq with outer coercion happens before inner coercion"
      (let [result (schema/coerce-value! {:type :seq :spec {:type :int} :coerce seq} "123")]
        (is (= [1 2 3] result)))
      (let [result (schema/coerce-value! {:type :seq :spec {:type :int} :coerce schema/->seq} "123")]
        (is (= [123] result)))
      (is (thrown? #?(:cljs js/Error :default Exception) (schema/coerce-value! {:type :seq :spec {:type :int} :coerce #(map inc %)} :123)))) ;; cljs will map over a string

    (testing "of entity"
      (let [result (schema/coerce pet {:species  "dog"
                                       :birthday now
                                       :length   "2.3"
                                       :teeth    24.2
                                       :name     "Fluff"
                                       :owner    "12345"
                                       :uuid     a-uuid})]
        (is (false? (schema/error? result)))
        (is (= "dog" (:species result)))
        (is (= now (:birthday result)))
        (is (float= 2.3 (:length result) 0.001))
        (is (= 24 (:teeth result)))
        (is (= "Fluffy" (:name result)))
        (is (= 12345 (:owner result)))
        (is (= a-uuid (:uuid result)))))

    (testing "of entity, nil values omitted"
      (let [result (schema/coerce pet {:name "Fido"})]
        (is (false? (schema/error? result)))
        (is (= "Fidoy" (:name result)))
        (is (not (contains? result :length)))
        (is (not (contains? result :species)))
        (is (not (contains? result :teeth)))
        (is (not (contains? result :birthday)))
        (is (not (contains? result :owner)))
        (is (not (contains? result :uuid)))))

    (testing "of entity level coercions"
      (let [schema (assoc pet :* {:stage-name {:type   :string
                                               :coerce #(str (:name %) " the " (:species %))}})
            result (schema/coerce schema valid-pet)]
        (is (= "Fluffyy the dog" (:stage-name result)))))

    (testing "coerces to nil"
      (let [schema (assoc-in pet [:name :coerce] {:coerce (constantly nil)})
            result (schema/coerce schema (dissoc valid-pet :name))]
        (is (not (contains? result :name)))))

    (testing "entity level coerces to nil"
      (let [schema (assoc pet :* {:name {:type :string :coerce (constantly nil)}})
            result (schema/coerce schema valid-pet)]
        (is (not (contains? result :name)))))

    (testing "removes extra fields"
      (let [crufty (assoc valid-pet :garbage "yuk!")
            result (schema/coerce pet crufty)]
        (is (nil? (:garbage result)))
        (is (not (contains? result :garbage))))))


  (deftest multi-field
    (testing "with nil value"
      (is (nil? (schema/coerce-value! {:type [:int]} nil))))

    (testing "with empty list"
      (is (= () (schema/coerce-value! {:type [:int]} ()))))

    (testing "entity - with an empty seq value"
      (let [result (schema/coerce pet {:colors []})]
        (is (= [] (:colors result))))))

  (testing "nested entity as a seq is coerced into a map"
    (let [result (schema/coerce pet {:parent [[:name "Fido"] [:age "12"]]})]
      (is (= {:name "Fido" :age 12} (:parent result)))))

  (testing "message is used"
    (let [result (schema/coerce pet {:length "foo"})]
      (is (= "must be unit in feet" (:length (schema/message-map result))))
      (is (= "can't coerce \"foo\" to float" (-> result :length schema/error-exception ex-message)))))

  (testing "message at entity level is used"
    (let [schema (assoc pet :* {:name {:coerce (fn [_] (throw (ex-info "blah" {}))) :message "boom!"}})
          result (schema/coerce schema valid-pet)]
      (is (= "boom!" (:name (schema/message-map result))))
      (is (= "blah" (-> result :name schema/error-exception ex-message)))))

  (testing "error info"
    (let [result (schema/coerce pet {:length "foo"})
          error  (:length result)]
      (is (true? (schema/field-error? error)))
      (is (= "must be unit in feet" (schema/error-message error)))
      (is (= "can't coerce \"foo\" to float" (-> error schema/error-exception ex-message)))
      (is (= "foo" (schema/error-value error)))
      (is (= "float" (schema/error-type error)))
      (is (= #{:exception :type :value} (set (keys (schema/error-data error))))))))



(deftest validation

  (testing "of presence"
    (is (false? (schema/present? nil)))
    (is (false? (schema/present? "")))
    (is (true? (schema/present? 1)))
    (is (true? (schema/present? "abc"))))

  (testing "of email?"
    (is (schema/email? "micahmartin@gmail.com"))
    (is (schema/email? "micah@clenacoders.com"))
    (is (schema/email? "vikas.rao@rsa.rohde-schwarz.com"))
    (is (not (schema/email? "micah@clenacoders")))
    (is (not (schema/email? "micah"))))

  (testing "of enum"
    (let [is-temperament? (schema/is-enum? temperaments)]
      (is (is-temperament? nil))
      (is (is-temperament? :temperament/wild))
      (is (is-temperament? :temperament/domestic))
      (is (not (is-temperament? ":temperament/savage")))
      (is (not (is-temperament? :wild)))
      (is (not (is-temperament? :temperament/savage)))))

  (deftest validation-from-spec

    (testing "with missing type"
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"invalid spec: \{\}"
            (schema/validate-value! {} 123))))

    (testing "of booleans"
      (is (true? (schema/valid-value? {:type :boolean} true)))
      (is (true? (schema/valid-value? {:type :boolean} false)))
      (is (false? (schema/valid-value? {:type :boolean} 123))))

    (testing "of strings"
      (is (true? (schema/valid-value? {:type :string} "123")))
      (is (false? (schema/valid-value? {:type :string} 123))))

    (testing "of keywords"
      (is (true? (schema/valid-value? {:type :keyword} :abc)))
      (is (false? (schema/valid-value? {:type :keyword} "abc")))
      (is (false? (schema/valid-value? {:type :keyword} 123))))


    (testing "of kw-ref"
      (is (true? (schema/valid-value? {:type :kw-ref} :abc)))
      (is (false? (schema/valid-value? {:type :kw-ref} "abc")))
      (is (false? (schema/valid-value? {:type :kw-ref} 123))))

    (testing "of int"
      (is (true? (schema/valid-value? {:type :int} 123)))
      (is (false? (schema/valid-value? {:type :int} 123.45)))
      (is (true? (schema/valid-value? {:type :long} 123)))
      (is (false? (schema/valid-value? {:type :long} 123.45))))

    (testing "of ref"
      (is (true? (schema/valid-value? {:type :ref} 123)))
      (is (false? (schema/valid-value? {:type :ref} 123.45))))

    (testing "of float"
      (is (true? (schema/valid-value? {:type :float} 123.456)))
      #?(:cljs    nil
         :default (is (false? (schema/valid-value? {:type :float} 123))))
      #?(:clj (is (false? (schema/valid-value? {:type :float} 123M))))
      (is (false? (schema/valid-value? {:type :float} "123")))
      (is (true? (schema/valid-value? {:type :double} 123.456)))
      #?(:cljs    nil
         :default (is (false? (schema/valid-value? {:type :double} 123))))
      #?(:clj (is (false? (schema/valid-value? {:type :double} 123M))))
      (is (false? (schema/valid-value? {:type :double} "123"))))

    (testing "of bigdec"
      (is (true? (schema/valid-value? {:type :bigdec} 123.456M)))
      #?(:clj (is (false? (schema/valid-value? {:type :bigdec} 123.456))))
      #?(:clj (is (false? (schema/valid-value? {:type :bigdec} 123))))
      (is (false? (schema/valid-value? {:type :bigdec} "123"))))

    (testing "of date/instant"
      (is (true? (schema/valid-value? {:type :instant} nil)))
      (is (false? (schema/valid-value? {:type :instant} "foo")))
      (is (false? (schema/valid-value? {:type :instant} 123)))
      (let [now #?(:clj (java.util.Date.) :cljs (js/Date.) :cljd (DateTime/now))]
        (is (true? (schema/valid-value? {:type :instant} now))))
      #?(:cljs (is (false? (schema/valid-value? {:type :instant} (js/goog.date.Date.))))))

    (testing "of sql-date"
      (is (true? (schema/valid-value? {:type :date} nil)))
      (is (false? (schema/valid-value? {:type :date} "foo")))
      (is (false? (schema/valid-value? {:type :date} 123)))
      #?(:clj (is (false? (schema/valid-value? {:type :date} (java.util.Date.)))))
      #?(:clj (is (true? (schema/valid-value? {:type :date} (java.sql.Date. (System/currentTimeMillis))))))
      #?(:cljd (is (true? (schema/valid-value? {:type :date} (DateTime/now)))))
      #?(:cljs (is (true? (schema/valid-value? {:type :date} (js/Date.)))))
      #?(:cljs (is (false? (schema/valid-value? {:type :date} (js/goog.date.Date.))))))

    (testing "of timestamp"
      (is (true? (schema/valid-value? {:type :timestamp} nil)))
      (is (false? (schema/valid-value? {:type :timestamp} "foo")))
      (is (false? (schema/valid-value? {:type :timestamp} 123)))
      #?(:clj (is (false? (schema/valid-value? {:type :timestamp} (java.util.Date.)))))
      #?(:clj (is (true? (schema/valid-value? {:type :timestamp} (java.sql.Timestamp. (System/currentTimeMillis))))))
      #?(:cljd (is (true? (schema/valid-value? {:type :timestamp} (DateTime/now)))))
      #?(:cljs (is (true? (schema/valid-value? {:type :timestamp} (js/Date.)))))
      #?(:cljs (is (false? (schema/valid-value? {:type :timestamp} (js/goog.date.Date.))))))

    (testing "of URI"
      (is (true? (schema/valid-value? {:type :uri} nil)))
      (is (#?(:cljs true? :default false?) (schema/valid-value? {:type :uri} "foo")))
      #?(:clj (is (true? (schema/valid-value? {:type :uri} (URI/create "foo")))))
      #?(:cljd (is (true? (schema/valid-value? {:type :uri} (Uri/new .path "foo")))))
      (is (false? (schema/valid-value? {:type :uri} 123))))

    (testing "of UUID"
      (is (true? (schema/valid-value? {:type :uuid} nil)))
      (is (false? (schema/valid-value? {:type :uuid} "foo")))
      (is (true? (schema/valid-value? {:type :uuid} a-uuid)))
      (is (false? (schema/valid-value? {:type :uuid} "1234")))
      (is (false? (schema/valid-value? {:type :uuid} 123))))

    (testing "of custom validation"
      (let [spec {:type :string :validate #(re-matches #"x+" %)}]
        (is (true? (schema/valid-value? spec "xxx")))
        (is (false? (schema/valid-value? spec "xox")))))

    (testing "of multiple custom validations"
      (let [spec {:type :string :validate [#(not (nil? %)) #(<= 5 (count %))]}]
        (is (true? (schema/valid-value? spec "abcdef")))
        (is (false? (schema/valid-value? spec nil)))))

    (testing "allows nils, unless specified"
      (is (true? (schema/valid-value? {:type :string} nil)))
      (is (false? (schema/valid-value? {:type :string :validate [schema/present?]} nil)))
      (is (true? (schema/valid-value? {:type :int} nil)))
      (is (false? (schema/valid-value? {:type :int :validate [schema/present?]} nil)))
      (is (true? (schema/valid-value? {:type :ref} nil)))
      (is (false? (schema/valid-value? {:type :ref :validate [schema/present?]} nil)))
      (is (true? (schema/valid-value? {:type :float} nil)))
      (is (false? (schema/valid-value? {:type :float :validate [schema/present?]} nil)))
      (is (true? (schema/valid-value? {:type :instant} nil)))
      (is (false? (schema/valid-value? {:type :instant :validate [schema/present?]} nil))))

    (testing "of seq with default validations"
      (is (true? (schema/valid-value? {:type [:float]} [32.1 3.1415])))
      (is (false? (schema/valid-value? {:type [:float]} 3.1415)))
      (is (false? (schema/valid-value? {:type [:float]} ["3.1415"])))
      (is (true? (schema/valid-value? {:type [:float]} nil))))

    (testing "of seq with custom validations"
      (is (true? (schema/valid-value? {:type [:float] :validate pos?} [32.1 3.1415])))
      (is (false? (schema/valid-value? {:type [:float] :validate pos?} [32.1 -3.1415]))))

    (testing "of object"
      (let [spec {:type {:foo {:type :keyword}}}]
        (is (true? (schema/valid-value? spec {:foo :bar})))))

    (testing "of seq of objects"
      (let [spec {:type [{:age {:type :int}}]}]
        (is (true? (schema/valid-value? spec [{:age 1} {:age 2}])))
        (is (false? (schema/valid-value? spec [{:age :foo}])))))

    (testing "of object with customs"
      (let [spec {:type {:foo {:type :keyword}} :validate :foo}]
        (is (true? (schema/valid-value? spec {:foo :bar})))
        (is (false? (schema/valid-value? spec {})))))

    (testing "of multiple object with custom validation"
      (let [spec {:type [{:foo {:type :keyword}}] :validate :foo}]
        (is (true? (schema/valid-value? spec [{:foo :bar} {:foo :baz}])))
        (is (false? (schema/valid-value? spec [{:foo :bar} {}])))
        (is (false? (schema/valid-value? spec [{} {:foo :bar}])))
        (is (true? (schema/valid-value? spec [])))))

    (testing "of object with nested validations"
      (let [spec {:type     {:foo   {:type :keyword}
                             :hello {:type :string :validate (partial = "world")}}
                  :validate :foo}]
        (is (false? (schema/valid-value? spec {:foo :bar})))
        (is (false? (schema/valid-value? spec {:hello "world"})))
        (is (false? (schema/valid-value? spec {:foo :bar :hello "worlds"})))
        (is (true? (schema/valid-value? spec {:foo :bar :hello "world"})))))

    (testing "of seq with outer validation happens after inner validation"
      (let [spec {:type :seq :spec {:type :int :validate pos? :message "neg"} :validate seq :message "empty"}]
        (is (= [123] (schema/-process-spec-on-value :validate spec [123])))
        (is (= "neg" (->> [-123] (schema/-process-spec-on-value :validate spec) first :message)))
        (is (= "empty" (->> [] (schema/-process-spec-on-value :validate spec) :message)))))

    (testing "missing multiple type coercer"
      (is (nil? (schema/validate-value! {:type [:blah]} nil)))
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"\[:int\] expected"
            (schema/validate-value! {:type [:int]} :foo)))
      (is (thrown-with-msg?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            #"unhandled validation type: :blah"
            (schema/validate-value! {:type [:blah]} [:foo]))))

    (testing "of invalid entity"
      (let [result (schema/validate pet invalid-pet)
            errors (schema/error-map result)]
        (is (true? (schema/error? result)))
        (is (= "must be a pet species" (schema/error-message (:species errors))))
        (is (= "must be a date" (schema/error-message (:birthday errors))))
        (is (= "must be unit in feet" (schema/error-message (:length errors))))
        (is (= "must be between 0 and 999" (schema/error-message (:teeth errors))))
        (is (= "must be nice and unique name" (schema/error-message (:name errors))))
        (is (= "must be a valid reference format" (schema/error-message (:owner errors))))
        (is (= "is invalid" (schema/error-message (:age (:parent errors)))))))

    (testing "of valid entity"
      (let [result (schema/validate pet valid-pet)]
        (is (false? (schema/error? result)))))

    (testing "of entity with missing(required) fields"
      (let [result   (schema/validate pet {})
            failures (schema/error-map result)]
        (is (true? (schema/error? result)))
        (is (contains? failures :owner))
        (is (not (contains? failures :birthday)))))

    (testing "of entity level validations"
      (let [spec    (assoc pet :* {:species {:validate #(not (and (= "snake" (:species %))
                                                                  (= "Fluffy" (:name %))))
                                             :message  "Snakes are not fluffy!"}})
            result1 (schema/validate spec valid-pet)
            result2 (schema/validate spec (assoc valid-pet :name "Fluffy" :species "snake"))]
        (is (false? (schema/error? result1)))
        (is (true? (schema/error? result2)))
        (is (= "Snakes are not fluffy!" (:species (schema/message-map result2))))))

    (testing ":validations validations/message pairs"
      (let [spec    (merge-with merge pet
                                {:species {:validate    nil
                                           :validations [{:validate nil? :message "species not nil"}]}
                                 :name    {:validate    nil
                                           :validations [{:validate [schema/present? #(= "blah" %)] :message "bad name"}]}})
            result1 (schema/validate spec (assoc valid-pet :species nil :name "blah"))
            result2 (schema/validate spec (assoc valid-pet :name "Fluffy" :species "snake"))]
        (is (false? (schema/error? result1)))
        (is (true? (schema/error? result2)))
        (is (= "species not nil" (:species (schema/message-map result2))))
        (is (= "bad name" (:name (schema/message-map result2))))))

    (testing "validations stop on first failure"
      (let [spec    (merge-with merge pet
                                {:species {:validate #(str/starts-with? % "s")
                                           :message  "not s species"
                                           :validations
                                           [{:validate #(str/ends-with? % "e") :message "not *e species"}
                                            {:validate #(= "snake" %) :message "not snake"}]}})
            result1 (schema/validate spec (assoc valid-pet :species "snake"))
            result2 (schema/validate spec (assoc valid-pet :species "swine"))
            result3 (schema/validate spec (assoc valid-pet :species "snail"))
            result4 (schema/validate spec (assoc valid-pet :species "crab"))]
        (is (false? (schema/error? result1)))
        (is (true? (schema/error? result2)))
        (is (= "not snake" (:species (schema/message-map result2))))
        (is (true? (schema/error? result3)))
        (is (= "not *e species" (:species (schema/message-map result3))))
        (is (true? (schema/error? result4)))
        (is (= "not s species" (:species (schema/message-map result4))))))

    (testing ":validation at entity level"
      (let [spec    (assoc pet :* {:species {:validations [{:validate #(not (and (= "snake" (:species %))
                                                                                 (= "Fluffy" (:name %))))
                                                            :message  "Snakes are not fluffy!"}]}})
            result1 (schema/validate spec valid-pet)
            result2 (schema/validate spec (assoc valid-pet :name "Fluffy" :species "snake"))]
        (is (false? (schema/error? result1)))
        (is (true? (schema/error? result2)))
        (is (= "Snakes are not fluffy!" (:species (schema/message-map result2))))))

    (testing "nested required field"
      (let [child  {:value {:type :string}}
            parent {:child {:type child :validations [{:validate some? :message "is required"}]}}]
        (is (= {:child "is required"} (schema/validate-message-map parent {})))))

    (testing "removes extra fields"
      (let [crufty (assoc valid-pet :garbage "yuk!")
            result (schema/validate pet crufty)]
        (is (nil? (:garbage result)))
        (is (not (contains? result :garbage))))))

  (testing "error info"
    (let [result (schema/validate pet {:species "frog"})
          error  (:species result)]
      (is (true? (schema/field-error? error)))
      (is (= "must be a pet species" (schema/error-message error)))
      (is (= "must be a pet species" (-> error schema/error-exception ex-message)))
      (is (= "frog" (schema/error-value error)))
      (is (nil? (schema/error-type error)))
      (is (= #{:exception :value} (set (keys (schema/error-data error))))))))


(deftest conforming

  (testing "with failed coercion"
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"can't coerce \"foo\" to int"
          (schema/conform-value! {:type :int} "foo")))
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"oh no!"
          (schema/conform-value! {:type :int :message "oh no!"} "foo"))))

  (testing "with failed validation"
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"oh no!"
          (schema/conform-value! {:type :int :validate even? :message "oh no!"} "123"))))

  (testing "of int the must be present"
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"is invalid"
          (schema/conform-value! {:type :int :validate [schema/present?]} "")))
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"is invalid"
          (schema/conform-value! {:type :long :validate schema/present?} ""))))

  (testing "success"
    (is (= 123 (schema/conform-value! {:type :int :message "oh no!"} "123"))))

  (testing "of sequentials"
    (is (= [123 321 3] (schema/conform-value! {:type [:int]} ["123.4" 321 3.1415]))))

  (testing "of sequentials - empty"
    (is (= [] (schema/conform-value! {:type [:int]} [])))
    (is (nil? (schema/conform-value! {:type [:int]} nil)))
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"\[:int\] expected"
          (schema/conform-value! {:type [:int]} "foo"))))

  (testing "of object"
    (let [spec {:type {:foo {:type :keyword}}}]
      (is (= {} (schema/conform-value! spec {})))
      (is (= {:foo :bar} (schema/conform-value! spec {:foo :bar :hello "world"})))))

  (testing "of multi object"
    (let [spec {:type [{:foo {:type :keyword}}]}]
      (is (= [{}] (schema/conform-value! spec [{}])))
      (is (= [{:foo :bar}] (schema/conform-value! spec [{:foo :bar :hello "world"}])))
      (is (nil? (schema/conform-value! spec nil)))))

  (testing "a valid entity"
    (let [result (schema/conform pet {:species  "dog"
                                      :birthday now
                                      :length   "2.3"
                                      :teeth    24.2
                                      :name     "Fluff"
                                      :owner    "12345"})]
      (is (false? (schema/error? result)))
      (is (= "dog" (:species result)))
      (is (= now (:birthday result)))
      (is (float= 2.3 (:length result) 0.001))
      (is (= 24 (:teeth result)))
      (is (= "Fluffy" (:name result)))
      (is (= 12345 (:owner result)))))

  (testing "entity - with an empty seq value"
    (let [result (schema/conform pet {:species  "dog"
                                      :birthday now
                                      :length   "2.3"
                                      :teeth    24.2
                                      :name     "Fluff"
                                      :owner    "12345"
                                      :colors   []})]
      (is (false? (schema/error? result)))
      (is (= [] (:colors result)))))

  (testing "of entity level operations"
    (let [spec    (assoc pet :* {:species {:type     :ignore
                                           :coerce   (constantly "snake")
                                           :validate #(not (and (= "snake" (:species %))
                                                                (= "Fluffyy" (:name %))))
                                           :message  "Snakes are not fluffy!"}})
          result1 (schema/conform spec (assoc valid-pet :name "Slimey"))
          result2 (schema/conform spec valid-pet)]
      (is (false? (schema/error? result1)))
      (is (= "snake" (:species result1)))
      (is (true? (schema/error? result2)))
      (is (= "Snakes are not fluffy!" (:species (schema/message-map result2))))))

  (testing "of entity level operations on nil values"
    (let [spec   (assoc pet
                   :* {:length {:validate #(or (nil? (:length %))
                                               (pos? (:length %)))
                                :message  "must be a positive number"}})
          result (schema/conform spec (dissoc valid-pet :length))]
      (is (false? (schema/error? result)))
      (is (not (contains? result :length)))))

  (testing "a invalid entity"
    (let [result (schema/conform pet invalid-pet)]
      (is (true? (schema/error? result)))
      (is (= "must be a pet species" (schema/error-message (:species result))))
      (is (= "must be a date" (schema/error-message (:birthday result))))
      (is (= "must be unit in feet" (schema/error-message (:length result))))
      (is (= "must be between 0 and 999" (schema/error-message (:teeth result))))
      (is (= "must be nice and unique name" (schema/error-message (:name result))))
      (is (= "must be a valid reference format" (schema/error-message (:owner result))))
      (is (= "can't coerce :foo to int" (schema/error-message (:age (:parent result)))))))

  (testing "removes extra fields"
    (let [crufty (assoc valid-pet :garbage "yuk!")
          result (schema/conform pet crufty)]
      (is (nil? (:garbage result)))
      (is (not (contains? result :garbage)))))

  (testing ":validations errors"
    (let [spec    (merge-with merge pet
                              {:species {:validate    nil
                                         :validations [{:validate nil? :message "species not nil"}]}
                               :name    {:validate    nil
                                         :coerce      nil
                                         :validations [{:validate [schema/present? #(= "blah" %)] :message "bad name"}]}})
          result1 (schema/conform spec (assoc valid-pet :species nil :name "blah"))
          result2 (schema/conform spec (assoc valid-pet :name "Fluffy" :species "snake"))]
      (is (false? (schema/error? result1)))
      (is (true? (schema/error? result2)))
      (is (= "species not nil" (:species (schema/message-map result2))))
      (is (= "bad name" (:name (schema/message-map result2))))))

  (testing "with invalid schema"
    (is (thrown-with-msg?
          #?(:clj  clojure.lang.ExceptionInfo
             :cljd cljd.core/ExceptionInfo
             :cljs cljs.core/ExceptionInfo)
          #"invalid spec: \{:type \"hi\"\}"
          (schema/conform {:foo {:type "hi"}} {:foo "bar"}))))

  (testing "coercing a string to seq"
    (let [path-schema {:path {:type :seq :coerce utilc/<-edn :spec {:type :map :schema {:foo {:type :string}}}}}
          entity      {:path (pr-str [{:foo "foo"}])}
          result      (schema/conform path-schema entity)]
      (is (nil? (schema/message-map result)))))

  (testing "coercing a string to map"
    (let [path-schema {:path {:type    :map
                              :coerce  utilc/<-edn
                              :message "oops"
                              :schema  {:foo {:type :string :validate str/blank? :coerce #(apply str (rest %))}}}}]
      (let [valid (schema/conform path-schema {:path (pr-str {:foo ""})})]
        (is (nil? (schema/message-map valid)))
        (is (= {:foo ""} (:path valid))))
      (let [invalid (schema/conform path-schema {:path (pr-str {:foo "foo"})})]
        (is (= "is invalid" (get-in (schema/message-map invalid) [:path :foo]))))
      (let [not-a-map (schema/conform path-schema {:path (pr-str "im-not-a-map")})]
        (is (= "oops" (get-in (schema/message-map not-a-map) [:path]))))
      (let [valid-nested (schema/conform path-schema {:path (pr-str {:foo "f"})})]
        (is (nil? (schema/message-map valid-nested))))))

  (testing "required map field"
    (let [schema {:thing {:type :map :schema {:field {:type :any}} :validations [schema/required]}}]
      (is (= "is required" (:thing (schema/conform-message-map schema {})))))))

(deftest error-messages

  (testing "are nil when there are none"
    (is (nil? (schema/message-map {}))))

  (testing "are only given for failed results"
    (is (= {:name "must be nice and unique name"}
           (-> {:name (schema/-process-error :validate {:message "must be nice and unique name"})}
               (schema/message-map)))))

  (testing "with missing message"
    (is (= {:foo "blah"} (schema/message-map {:foo (schema/-process-error :validate {:exception (ex-info "blah" {})})}))))

  (testing "does not validate nil values against schema types"
    (let [jerry {:name "Jerry" :pet nil}]
      (is (nil? (schema/message-map (schema/coerce owner jerry))))
      (is (nil? (schema/message-map (schema/validate owner jerry))))
      (is (nil? (schema/message-map (schema/conform owner jerry))))))

  (testing "validates false values against schema types"
    (let [jerry {:name "Jerry" :pet false}]
      (is (= {:pet "can't coerce false to map"} (schema/message-map (schema/coerce owner jerry))))
      (is (= {:pet "is invalid"} (schema/message-map (schema/validate owner jerry))))
      (is (= {:pet "can't coerce false to map"} (schema/message-map (schema/conform owner jerry))))))

  (testing "does not require collection on seq of schema types"
    (let [house {:size 10 :pets nil}]
      (is (nil? (schema/message-map (schema/coerce household house))))
      (is (nil? (schema/message-map (schema/validate household house))))
      (is (nil? (schema/message-map (schema/conform household house))))))

  (testing "for single, top-level error"
    (let [invalid-pet (assoc valid-pet :name "")]
      (is (nil? (schema/message-map (schema/coerce pet invalid-pet))))
      (is (= {:name "must be nice and unique name"} (schema/message-map (schema/validate pet invalid-pet))))
      (is (= {:name "must be nice and unique name"} (schema/message-map (schema/conform pet invalid-pet))))))

  (testing "for multiple, top-level errors"
    (let [invalid-pet (assoc valid-pet :name 123 :species :cat)]
      (is (= {:name "must be nice and unique name", :species "must be a pet species"}
             (schema/message-map (schema/validate pet invalid-pet))))))

  (testing "specifies idx when inside sequential structure"
    (let [invalid-pet {:species  "dog"
                       :birthday now
                       :length   2.5
                       :teeth    24
                       :name     "Fluffy"
                       :owner    12345
                       :colors   ["brown" "white" 123 "red" 456]
                       :uuid     a-uuid}]
      (is (= {:colors {2 "must be a string"
                       4 "must be a string"}}
             (schema/message-map (schema/validate pet invalid-pet))))
      (is (nil? (schema/message-map (schema/validate pet valid-pet))))))

  (testing "specifies individual errors within nested entities"
    (let [invalid-owner {:pet invalid-pet}
          valid-owner   {:pet valid-pet}]
      (is (= {:pet {:parent   {:age "is invalid"}
                    :name     "must be nice and unique name"
                    :species  "must be a pet species"
                    :birthday "must be a date"
                    :teeth    "must be between 0 and 999"
                    :length   "must be unit in feet"
                    :owner    "must be a valid reference format"}}
             (schema/message-map (schema/validate owner invalid-owner))))
      (is (nil? (schema/message-map (schema/validate owner valid-owner))))))

  (testing "specifies idx for invalid nested entity inside sequential structure"
    (let [invalid-household {:pets [valid-pet invalid-pet valid-pet invalid-pet]}
          error             {:parent   {:age "is invalid"}
                             :name     "must be nice and unique name"
                             :species  "must be a pet species"
                             :birthday "must be a date"
                             :teeth    "must be between 0 and 999"
                             :length   "must be unit in feet"
                             :owner    "must be a valid reference format"}]
      (is (= {:pets {1 error 3 error}} (schema/message-map (schema/validate household invalid-household))))))

  (testing "message-seq flat"
    (let [result (schema/message-seq (schema/conform pet (dissoc invalid-pet :parent)))]
      (is (some #{"name must be nice and unique name"} result))
      (is (some #{"species must be a pet species"} result))
      (is (some #{"birthday must be a date"} result))
      (is (some #{"teeth must be between 0 and 999"} result))
      (is (some #{"length must be unit in feet"} result))
      (is (some #{"owner must be a valid reference format"} result))))

  (testing "message-seq nested"
    (let [invalid-household {:pets [valid-pet invalid-pet valid-pet invalid-pet]}
          result            (schema/message-seq (schema/conform household invalid-household))]
      (is (some #{"pets.1.parent.age can't coerce :foo to int"} result))
      (is (some #{"pets.1.name must be nice and unique name"} result))
      (is (some #{"pets.3.parent.age can't coerce :foo to int"} result))
      (is (some #{"pets.3.name must be nice and unique name"} result)))))

(deftest presentation

  (testing "of int"
    (is (= 123 (schema/present-value! {:type :int} 123)))
    (is (= 123 (schema/present-value! {:type :long} 123))))

  (testing "of float"
    (is (= 12.34 (schema/present-value! {:type :float} 12.34)))
    (is (= 12.34 (schema/present-value! {:type :double} 12.34))))

  (testing "of string"
    (is (= "foo" (schema/present-value! {:type :string} "foo"))))

  (testing "of date"
    (is (= now (schema/present-value! {:type :instant} now))))

  (testing "applies custom presenter"
    (is (= 124 (schema/present-value! {:type :long :present inc} 123))))

  (testing "ommited"
    (is (nil? (schema/present-value! {:type :long :present schema/omit} 123))))

  (testing "applies multiple custom presenters"
    (is (= 62 (schema/present-value! {:type :long :present [inc #(/ % 2)]} 123))))

  (testing "of sequentials"
    (is (= [123 456] (schema/present-value! {:type [:int]} [123 456]))))

  (testing "of sequentials - empty"
    (is (= [] (schema/present-value! {:type [:int]} []))))

  (testing "of sequentials - nil"
    (is (nil? (schema/present-value! {:type [:int]} nil))))

  (testing "of sequentials with customs"
    (is (= ["123" "456"] (schema/present-value! {:type [:int] :present str} [123 456])))
    (is (= ["2" "3" "4" "5"] (schema/present-value! {:type [:float] :present [inc str]} [1 2 3 4]))))

  (testing "of sequentials when omitted"
    (is (= [] (schema/present-value! {:type [:int] :present schema/omit} [123 456]))))

  (testing "of object"
    (let [spec  {:type {:age {:type :int :present str}}}
          value {:age 10}]
      (is (= {:age "10"} (schema/present-value! spec value)))))

  (testing "of sequential object"
    (let [spec  {:type [{:age {:type :int :present str}}]}
          value [{:age 10}]]
      (is (= [{:age "10"}] (schema/present-value! spec value)))))

  (testing "of object with customs"
    (let [spec  {:type    {:age {:type :int}}
                 :present pr-str}
          value {:age 10}]
      (is (= "{:age 10}" (schema/present-value! spec value)))))

  (testing "of object with presentable attributes"
    (let [spec  {:type    {:age {:type :int :present inc}}
                 :present pr-str}
          value {:age 10}]
      (is (= "{:age 11}" (schema/present-value! spec value)))))

  (deftest presentation-of-entity

    (testing "doesn't present omitted (nil) results"
      (let [schema (assoc-in pet [:owner :present] schema/omit)
            result (schema/present schema (assoc valid-pet :owner "George"))]
        (is (not (contains? result :id)))
        (is (not (contains? result :owner)))))

    (testing "with entity level presentation"
      (let [result (schema/present (assoc pet :* {:stage-name {:present #(str (:name %) " the " (:species %))}}) valid-pet)]
        (is (= "Fluffy the dog" (:stage-name result)))))

    (testing "with error on entity level presentation"
      (let [result (schema/present (assoc pet :* {:stage-name {:present #(throw (ex-info "blah" {:x %}))}}) valid-pet)]
        (is (true? (schema/error? result)))
        (is (contains? (schema/error-map result) :stage-name))))

    (testing "with error on entity level presentation!"
      (is (thrown?
            #?(:clj  clojure.lang.ExceptionInfo
               :cljd cljd.core/ExceptionInfo
               :cljs cljs.core/ExceptionInfo)
            (schema/present!
              (assoc pet :* {:stage-name {:present #(throw (ex-info "blah" {:x %}))}}) valid-pet))))))

(deftest kind

  (testing "is enforced on validate!"
    (let [result (schema/validate pet (assoc valid-pet :kind :beast))
          kind   (:kind result)]
      (is (true? (schema/field-error? kind)))
      (is (true? (schema/error? result)))
      (is (= ["kind mismatch; must be :pet"] (schema/message-seq result)))))

  (testing "can be left out"
    (is (false? (schema/error? (schema/validate pet (dissoc valid-pet :kind))))))

  (testing "will be added if missing by conform"
    (let [result (schema/conform pet (dissoc valid-pet :kind))]
      (is (false? (schema/error? result)))
      (is (= :pet (:kind result))))))

(deftest entity-level

  (testing "must be maps"
    (is (= "can't coerce \"foo\" to map" (schema/error-message (schema/coerce pet "foo"))))
    (is (= "can't coerce \"foo\" to map" (schema/error-message (schema/validate pet "foo"))))
    (is (= "can't coerce \"foo\" to map" (schema/error-message (schema/conform pet "foo"))))
    (is (= "can't coerce \"foo\" to map" (schema/error-message (schema/present pet "foo")))))

  (testing "errors don't get added until after"
    (let [schema {:foo {:type :string}
                  :bar {:type :string}
                  :*   {:foo {:validate seq}
                        :bar {:validate seq}}}]
      (is (= {:foo "is invalid" :bar "is invalid"} (schema/validate-message-map schema {}))))))

(deftest merge-schemas

  (testing "simple"
    (let [pet-a  {:kind    (schema/kind :pet)
                  :id      schema/id
                  :name    {:type :string}
                  :species {:type :string :validate :valid-species :message "invalid species"}
                  :*       {:name {:validate :valid-entity-name}}}
          pet-b  {:name    {:validate :valid-name :message "invalid name"}
                  :species {:coerce :coerce-species}
                  :color   {:type :string}
                  :*       {:species {:validate :valid-entity-species}}}
          result (schema/merge-schemas pet-a pet-b)]
      (is (= schema/id (:id result)))
      (is (= {:type        :string :message "invalid name"
              :validations [{:validate :valid-name, :message "invalid name"}]}
             (:name result)))
      (is (= {:type        :string :message "invalid species"
              :validations [{:validate :valid-species, :message "invalid species"}]
              :coerce      :coerce-species} (:species result)))
      (is (= {:type :string} (:color result)))
      (is (= {:name    {:validate :valid-entity-name}
              :species {:validate :valid-entity-species}}
             (:* result)))))

  (testing "with validations"
    (let [pet-a  {:kind    (schema/kind :pet)
                  :id      schema/id
                  :name    {:type :string}
                  :species {:type :string :validations [{:validate :valid-species :message "invalid species"}]}
                  :*       {:name {:validations [{:validate :valid-entity-name}]}}}
          pet-b  {:name    {:validations [{:validate :valid-name :message "invalid name"}]}
                  :species {:validations [{:validate :valid-species2 :message "invalid2 species"}]}
                  :*       {:species {:validations [{:validate :valid-entity-species}]}
                            :name    {:validations [{:validate :valid-entity-name2}]}}}
          result (schema/merge-schemas pet-a pet-b)]
      (is (= schema/id (:id result)))
      (is (= {:type :string :validations [{:validate :valid-name :message "invalid name"}]} (:name result)))
      (is (= {:type        :string
              :validations [{:validate :valid-species :message "invalid species"}
                            {:validate :valid-species2 :message "invalid2 species"}]}
             (:species result)))
      (is (= {:species {:validations [{:validate :valid-entity-species}]}
              :name    {:validations [{:validate :valid-entity-name}
                                      {:validate :valid-entity-name2}]}} (:* result)))))

  (testing "conflicting validate"
    (let [pet-a  {:kind    (schema/kind :pet)
                  :id      schema/id
                  :species {:type :string :validate :valid-species :message "invalid species"}
                  :*       {:species {:validate :valid-entity-species :message "invalid entity species"}}}
          pet-b  {:species {:type :string :validate :valid-species2 :message "invalid species2"}
                  :*       {:species {:validate :valid-entity-species2 :message "invalid entity species2"}}}
          result (schema/merge-schemas pet-a pet-b)]
      (is (= schema/id (:id result)))
      (is (= {:type        :string
              :validations [{:validate :valid-species :message "invalid species"}
                            {:validate :valid-species2 :message "invalid species2"}]
              :message     "invalid species2"} (:species result)))

      (is (= {:species {:message     "invalid entity species2"
                        :validations [{:validate :valid-entity-species :message "invalid entity species"}
                                      {:validate :valid-entity-species2 :message "invalid entity species2"}]}}
             (:* result))))))

(deftest one-of

  (testing "no specs"
    (let [spec          {:type :one-of}
          coerce-result (schema/-process-spec-on-value :coerce spec 1)]
      (is (true? (schema/error? coerce-result)) (pr-str coerce-result))
      (is (= "one-of: empty specs" (:message coerce-result)))))

  (testing "no choice"
    (let [spec          {:type :one-of :specs []}
          coerce-result (schema/-process-spec-on-value :coerce spec 1)]
      (is (true? (schema/error? coerce-result)))
      (is (= "one-of: empty specs" (:message coerce-result)))))

  (testing "one choice - coerce"
    (let [spec {:type :one-of :specs [{:type :int}]}]
      (is (nil? (schema/-process-spec-on-value :coerce spec nil)))
      (is (= 1 (schema/-process-spec-on-value :coerce spec 1)))
      (is (= 2 (schema/-process-spec-on-value :coerce spec "2")))
      (is (= "one-of: no matching spec" (:message (schema/-process-spec-on-value :coerce spec "blah"))))))

  (testing "one choice - validate"
    (let [spec {:type :one-of :specs [{:type :int :validate pos?}]}]
      (is (= 1 (schema/-process-spec-on-value :validate spec 1)))
      (is (= 2 (schema/-process-spec-on-value :validate spec 2)))
      (is (= "one-of: no matching spec" (:message (schema/-process-spec-on-value :validate spec -3))))))

  (testing "multiple choices"
    (let [spec {:type :one-of :specs [{:type :int :validate even?}
                                      {:type :int :validate pos?}
                                      {:type :string :validate #{"foo" "bar"}}]}]
      (is (= 1 (schema/-process-spec-on-value :conform spec 1)))
      (is (= -2 (schema/-process-spec-on-value :conform spec -2)))
      (is (= 3 (schema/-process-spec-on-value :conform spec "3")))
      (is (= "one-of: no matching spec" (:message (schema/-process-spec-on-value :conform spec -5))))
      (is (= "one-of: no matching spec" (:message (schema/-process-spec-on-value :conform spec "blah")))))))

(deftest shorthands

  (testing "none"
    (is (= {:type :int} (schema/normalize-spec {:type :int})))
    (is (= {:type :string :message "foo"} (schema/normalize-spec {:type :string :message "foo"}))))

  (testing "invalid"
    (is (thrown? #?(:cljs cljs.core/ExceptionInfo
                    :clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (schema/normalize-spec nil)))
    (is (thrown? #?(:cljs cljs.core/ExceptionInfo
                    :clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (schema/normalize-spec 1)))
    (is (thrown? #?(:cljs cljs.core/ExceptionInfo
                    :clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (schema/normalize-spec {:type nil})))
    (is (thrown? #?(:cljs cljs.core/ExceptionInfo
                    :clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (schema/normalize-spec {:type 1})))
    (is (thrown? #?(:cljs cljs.core/ExceptionInfo
                    :clj  clojure.lang.ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (schema/normalize-spec {:type now}))))

  (testing "keyword"
    (is (= {:type :string} (schema/normalize-spec :string)))
    (is (= {:type :long} (schema/normalize-spec :long)))
    (is (= {:type :ignore} (schema/normalize-spec :ignore)))
    (is (= {:type :instant} (schema/normalize-spec :instant)))
    (is (= {:type :blah} (schema/normalize-spec :blah))))

  (deftest seq-type

    (testing "errors"
      (is (thrown? #?(:cljs cljs.core/ExceptionInfo :clj clojure.lang.ExceptionInfo :cljd cljd.core/ExceptionInfo) (schema/normalize-spec {:type [:int :int]})))
      (is (thrown? #?(:cljs cljs.core/ExceptionInfo :clj clojure.lang.ExceptionInfo :cljd cljd.core/ExceptionInfo) (schema/normalize-spec {:type [:int :string]})))
      (is (thrown? #?(:cljs cljs.core/ExceptionInfo :clj clojure.lang.ExceptionInfo :cljd cljd.core/ExceptionInfo) (schema/normalize-spec {:type []}))))

    (testing "with type"
      (let [result (schema/normalize-spec {:type [:int]})]
        (is (= {:type :seq :spec {:type :int}} result)))
      (let [result (schema/normalize-spec {:type [:int] :validate even? :foo "bar"})]
        (is (= {:type :seq :spec {:type :int :validate even?} :foo "bar"} result))))

    (testing "with spec"
      (let [result (schema/normalize-spec {:type [{:type :int}] :message "foo"})]
        (is (= {:type :seq :spec {:type :int} :message "foo"} result)))
      (let [result (schema/normalize-spec {:type [{:type :int}] :message "foo" :foo "bar"})]
        (is (= {:type :seq :spec {:type :int} :message "foo" :foo "bar"} result))))

    (testing "with schema"
      (let [result (schema/normalize-spec {:type [{:foo "bar"}]})]
        (is (= {:type :seq :spec {:type :map :schema {:foo "bar"}}} result)))
      (let [result (schema/normalize-spec {:type [{:foo "bar"}] :validate :foo})]
        (is (= {:type :seq :spec {:type :map :schema {:foo "bar"} :validate :foo}} result)))
      (let [result (schema/normalize-spec {:type [{:foo "bar"}] :foo "bar"})]
        (is (= {:type :seq :spec {:type :map :schema {:foo "bar"}} :foo "bar"} result)))))

  (testing "map"
    (let [result (schema/normalize-spec {:type {:foo {:type :string}}})]
      (is (= {:type :map :schema {:foo {:type :string}}} result)))
    (let [result (schema/normalize-spec {:type {:foo {:type :string}} :validate map?})]
      (is (= {:type :map :schema {:foo {:type :string}} :validate map?} result))))

  (testing "set"
    (let [result (schema/normalize-spec {:type #{:int :string}})]
      (is (= {:type :one-of :specs [{:type :int} {:type :string}]} result))))

  (deftest normalize-schema

    (testing "pet"
      (let [result (schema/normalize-schema pet)]
        (is (= (dissoc pet :colors :parent) (dissoc result :colors :parent)))
        (is (= {:type :seq :spec {:type :string :message "must be a string"}} (:colors result)))
        (is (= {:type :map, :schema {:name {:type :string}, :age {:type :int}}} (:parent result)))))

    (testing "with entity level spec"
      (let [pet    (assoc pet :* {:name {:validate :name}})
            result (schema/normalize-schema pet)]
        (is (= {:name {:validate :name}} (:* result)))))

    (testing "meta-data, skipping normalization"
      (let [result (schema/normalize-schema pet)]
        (is (= {:c3kit.apron.schema/normalized? true} (meta result)))
        #?(:cljd nil
           :default
           (let [invocations (atom 0)]
             (with-redefs [update-vals (fn [_m _f] (swap! invocations inc))]
               (is (= result (schema/normalize-schema result)))
               (is (zero? @invocations)))))))))

(deftest ignore-any

  (testing "ignore"
    (is (= :blah (schema/coerce-value! {:type :ignore} :blah))))

  (testing "any"
    (is (= :blah (schema/coerce-value! {:type :any} :blah)))))

(deftest spec-schema

  (testing "pets"
    (doseq [[_field spec] pet]
      (let [spec   (schema/normalize-spec spec)
            result (schema/conform schema/spec-schema spec)]
        (is (nil? (schema/message-map result))))))

  (testing "type"
    (is (= {:type "is required"} (schema/validate-message-map schema/spec-schema {:type nil})))
    (is (= {:type "must be one of schema/valid-types"} (schema/validate-message-map schema/spec-schema {:type :blah}))))

  (testing "validate"
    (is (= {:validate "must be an ifn or seq of ifn"}
           (schema/validate-message-map schema/spec-schema {:type :string :validate "blah"}))))

  (testing "coerce"
    (is (= {:coerce "must be an ifn or seq of ifn"}
           (schema/validate-message-map schema/spec-schema {:type :string :coerce "blah"}))))

  (testing "present"
    (is (= {:present "must be an ifn or seq of ifn"}
           (schema/validate-message-map schema/spec-schema {:type :string :present "blah"}))))

  (testing "message"
    (is (= ":blah" (:message (schema/coerce schema/spec-schema {:type :string :message :blah})))))

  (testing "validations"
    (is (= {:validations "[:map] expected"}
           (schema/validate-message-map schema/spec-schema {:type :string :validations "blah"})))
    (is (nil? (schema/validate-message-map schema/spec-schema {:type :string :validations []})))
    (is (= {:validations {0 "must be schema/validation-schema"}}
           (schema/validate-message-map schema/spec-schema {:type :string :validations [:blah]})))
    (is (= {:validations {0 {:validate "must be an ifn or seq of ifn"}}}
           (schema/validate-message-map schema/spec-schema {:type :string :validations [{:validate "blah"}]}))))

  (testing "spec"
    (is (= {:spec "only used with type :seq"}
           (schema/validate-message-map schema/spec-schema {:type :string :spec {:type :string}})))
    (is (= {:spec "must be schema/spec-schema"}
           (schema/validate-message-map schema/spec-schema {:type :seq :spec "blah"}))))

  (testing "specs"
    (is (= {:specs "only used with type :one-of"}
           (schema/validate-message-map schema/spec-schema {:type :string :specs [{:type :string}]})))
    (is (= {:specs "[:map] expected"}
           (schema/validate-message-map schema/spec-schema {:type :one-of :specs "blah"}))))

  (testing "schema"
    (is (= {:schema "only used with type :map"}
           (schema/validate-message-map schema/spec-schema {:type :string :schema {:foo {:type :string}}})))
    (is (= {:schema "must be a map"}
           (schema/validate-message-map schema/spec-schema {:type :map :schema "blah"}))))

  (deftest conform-schema

    (testing "pets with entity-level"
      (let [new-pet (assoc pet :* {:species {:validate #(not (and (= "snake" (:species %)) (= "Fluffy" (:name %))))
                                             :message  "Snakes are not fluffy!"}})
            schema  (schema/conform-schema! new-pet)]
        (is (contains? (:* schema) :species))))

    (testing "specs are normalized"
      (let [schema (schema/conform-schema! pet)]
        (is (= :seq (:type (:colors schema))))))

    (testing "extra spec attributes are not removed"
      (let [schema (schema/conform-schema! pet)]
        (is (= :pet (-> schema :kind :value)))))))
