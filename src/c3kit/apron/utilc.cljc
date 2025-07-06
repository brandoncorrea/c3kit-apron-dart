(ns c3kit.apron.utilc
  #?@(:cljd ()
      :clj  ((:import (java.util UUID)
                      (java.io ByteArrayInputStream ByteArrayOutputStream))))
  (:require #?(:cljd ["dart:convert" :as convert]
               :clj  [clojure.data.json :as json])
            [c3kit.apron.corec :as ccc]
            [c3kit.apron.schema :as schema]
            [clojure.edn :as edn]
            [clojure.string :as str]
            #?(:cljd ["package:transit_dart/transit_dart.dart" :as td])
            #?(:cljd    [wevre.transit-cljd :as transit]
               :default [cognitect.transit :as transit])))

(defn ->edn
  "Convenience.  Convert the form to EDN"
  [v] (some-> v pr-str))

(defn <-edn
  ^{:doc    "Convenience.  Convert the EDN string to a Clojure form"
    :inline (fn [s] `(edn/read-string ~s))}
  [s] (edn/read-string s))

(defn ->hex
  "Convert integer to a hex string"
  [n] #?(:clj  (format "%x" n)
         :cljd (.toRadixString ^int n 16)
         :cljs (.toString n 16)))

(defn <-hex
  "Convert hex string to an integer"
  [hex] #?(:clj  (Long/parseLong hex 16)
           :cljd (int/parse ^String hex .radix 16)
           :cljs (js/parseInt hex 16)))

(defn index-by-id
  "Give a list of entities with unique :id's, return a map with the ids as keys and the entities as values"
  [entities]
  (reduce #(assoc %1 (:id %2) %2) {} entities))

(defn keywordize-kind
  "Makes sure and entity has the keyword as the value of :kind"
  [entity]
  (if-let [kind (:kind entity)]
    (cond
      (keyword? kind) entity
      (string? kind) (assoc entity :kind (keyword kind))
      :else (throw (ex-info "Invalid :kind type" entity)))
    (throw (ex-info "Missing :kind" entity))))

(defn ->uuid-or-nil
  "Parse a string into a UUID or return nil if it's not a vlid UUID format"
  [uuid-str]
  (try (schema/->uuid uuid-str)
       (catch #?(:cljs :default :default Exception) _)))

; ----- Transit -----

#?(:cljs (def transit-reader (transit/reader :json {:handlers {"f" js/parseFloat "n" js/parseInt}})))
#?(:cljs (def transit-writer (transit/writer :json)))

#?(:cljd
   (defn- make-reader [f]
     (reify td/ReadHandler (fromRep [_ o] (f o)))))

#?(:cljd
   (def transit-opts
     {:custom-read-handlers
      {"u" (make-reader uuid)
       "f" (make-reader double/parse)
       "n" (make-reader int/parse)}
      :custom-write-handlers
      {(#/(td/Class cljd.core/UUID))
       (reify td/WriteHandler
         (tag [_ o] "u")
         (rep [_ o .tag] (str o))
         (stringRep [_ o] (str o)))}}))

#?(:cljd (def ^:private transit-codec (transit/json transit-opts)))
#?(:cljd (def ^:private transit-reader (.-decoder ^td/TransitJsonCodec transit-codec)))
#?(:cljd (def ^:private transit-writer (.-encoder ^td/TransitJsonCodec transit-codec)))

(defn ->transit
  "Convert data into transit string"
  ([type opts data]
   #?(:clj  (let [baos   (ByteArrayOutputStream.)
                  writer (transit/writer baos type opts)]
              (transit/write writer data)
              (.close baos)
              (.toString baos))
      :cljd (let [opts    {:custom-write-handlers (:handlers opts)}
                  options (merge transit-opts opts)
                  writer  (.-encoder (transit/json options))]
              (.convert writer data))
      :cljs (transit/write (transit/writer type opts) data)))
  ([data]
   #?(:clj  (->transit :json {} data)
      :cljd (.convert ^convert/Converter transit-writer data)
      :cljs (transit/write transit-writer data))))

(defn <-transit
  "Convert transit string into data"
  ([type opts ^String transit-str]
   #?(:clj  (with-open [in (ByteArrayInputStream. (.getBytes transit-str))]
              (transit/read (transit/reader in type opts)))
      :cljd (let [opts    {:custom-read-handlers (:handlers opts)}
                  options (merge transit-opts opts)
                  reader  (.-decoder (transit/json options))]
              (.convert reader transit-str))
      :cljs (transit/read (transit/reader type opts) transit-str)))
  ([^String transit-str]
   #?(:clj  (<-transit :json {} transit-str)
      :cljd (.convert ^convert/Converter transit-reader transit-str)
      :cljs (transit/read transit-reader transit-str))))

; ^^^^^ Transit ^^^^^

; ----- JSON -----

#?(:cljd
   (defn- clj->dart [v]
     (cond
       (map? v) (reduce-kv
                  (fn [^Map m k v] (doto m (. "[]=" (name k) (clj->dart v))))
                  (Map.)
                  v)
       (coll? v) (into-array v)
       :else v)))

(defn ->json
  "Convert the clj data structure to JSON.
  Note: this transition may be lossy since some clj data types (keywords) have no JSON equivalent."
  [v]
  #?(:clj  (json/write-str v)
     :cljd (convert/jsonEncode (clj->dart v))
     :cljs (.stringify js/JSON (clj->js v))))

#?(:cljd
   (defn- dart->clj [v & {:keys [keywordize-keys] :as opts}]
     (cond
       (instance? Map v)
       (reduce (fn [m [k v]]
                 (let [k (cond-> k keywordize-keys keyword)]
                   (assoc m k (dart->clj v opts)))) {} v)
       (instance? List v) (vec v)
       :else v)))

(defn <-json
  "Convert JSON into clj data structure."
  [v]
  (when (some-> v ccc/not-blank?)
    #?(:clj  (json/read-str v)
       :cljd (dart->clj (convert/jsonDecode v))
       :cljs (js->clj (.parse js/JSON v)))))

(defn <-json-kw
  "Convert JSON into clj data structure with all keys as keywords"
  [v]
  #?(:clj  (json/read-str v :key-fn keyword)
     :cljd (dart->clj (convert/jsonDecode v) :keywordize-keys true)
     :cljs (js->clj (.parse js/JSON v) :keywordize-keys true)))

; ^^^^^ JSON ^^^^^

; ----- CSV -----

(defn- csv-maybe-quote [value]
  (if (re-find #"[,\"]" value)
    (str "\"" (str/replace value "\"" "\"\"") "\"")
    value))

(defn cell->csv [cell]
  (-> cell str csv-maybe-quote))
(defn row->csv [row]
  (str/join "," (map cell->csv row)))

(defn ->csv
  "Simple CSV generator for a list of lists"
  [rows]
  (str/join "\r\n" (map row->csv rows)))

; ^^^^^ CSV ^^^^^

(defn ->filename
  "Sanatize string into valid filename"
  ([name] (-> (str name)
              (str/replace #"[ -]" "_")
              (str/replace #"[',.-/\\<>:\"\\|?*\[\]]" "")))
  ([name ext] (str (->filename name) "." ext)))
