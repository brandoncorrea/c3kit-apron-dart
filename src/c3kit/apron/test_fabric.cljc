(ns c3kit.apron.test-fabric
  "Truth is like the sun. You can shut it out for a time, but it ain't goin' away."
  (:require [clojure.string :as str]))

(defn- ->pattern [s]
  (-> s
      (str/replace "%" ".*")
      (str/replace "_" ".")
      re-pattern))

(defn- multi? [v] (or (sequential? v) (set? v)))

(defn- make-tester [pred]
  (fn [v]
    (if (multi? v)
      (some pred v)
      (pred v))))

(defn- pattern-matches? [pattern v case-sensitive?]
  (when v
    (let [v (cond-> v (not case-sensitive?) str/upper-case)]
      (boolean (re-matches pattern v)))))

(defn- pattern-comparator [v case-sensitive?]
  (let [pattern (->pattern v)]
    (make-tester #(pattern-matches? pattern % case-sensitive?))))

(defn- -normal-tester [f v]
  (make-tester #(some-> % (f v))))

(defn- -or-tester [values]
  (let [values (set values)]
    (make-tester #(contains? values %))))

(defn- -tester [[operation v :as form]]
  (condp = operation
    '> (if (number? v) (-normal-tester > v) (-normal-tester (comp pos? compare) v))
    '< (if (number? v) (-normal-tester < v) (-normal-tester (comp neg? compare) v))
    '>= (if (number? v) (-normal-tester >= v) (-normal-tester (comp not neg? compare) v))
    '<= (if (number? v) (-normal-tester <= v) (-normal-tester (comp not pos? compare) v))
    'like (pattern-comparator v true)
    'ilike (pattern-comparator (str/upper-case v) false)
    'not= (complement (-or-tester (rest form)))
    '= (-or-tester (rest form))
    (-or-tester form)))

(defn- ensure-key [k]
  (if (set? k)
    (map ensure-key k)
    (->> [(namespace k) (name k)]
         (remove nil?)
         (mapv keyword))))

(defn- get-tester-by-type [v]
  (cond (set? v) (-or-tester v)
        (sequential? v) (-tester v)
        (nil? v) nil?
        :else (-normal-tester = v)))

(defn- kv->tester [[k v]]
  (let [tester   (get-tester-by-type v)
        key-path (ensure-key k)]
    (fn [e] (tester (get-in e key-path)))))

(defn spec->tester
  "Creates a predicate based on the key-value pairs in spec."
  [spec]
  (apply every-pred (map kv->tester spec)))
