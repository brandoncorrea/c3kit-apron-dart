(ns c3kit.apron.corec
  "Common core code.  This file should have minimal dependencies.
  Clients should be able to safely :refer :all from this namespace."
  #?@(:cljd () :clj ((:import (java.util UUID))))
  #?(:cljs (:require-macros [c3kit.apron.corec :refer [for-all nand nor xor]]))
  (:require [c3kit.apron.test-fabric :as fabric]
            [clojure.string :as str]
            #?(:cljd ["package:sprintf/sprintf.dart" :as sprintf])
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.object :as gobj])))

(defmacro for-all [bindings body]
  `(doall (for ~bindings ~body)))

(defn map-all
  "Like for-all, but with map"
  ([f coll] (doall (map f coll)))
  ([f coll & colls] (doall (apply map f coll colls))))

#?(:cljs
   (defn map-component
     "Creates a collection where each element is put into component syntax: [component-fn item]
      Each component is given a React key using (key-fn item)."
     [key-fn component-fn coll]
     (map-all (fn [x] ^{:key (key-fn x)} [component-fn x]) coll)))

(defn map-all-indexed
  "Like for-all, but with map-indexed"
  [f coll] (doall (map-indexed f coll)))

#?(:cljs nil
   :default
   (defmacro nand
     "Same as (not (and ...))"
     ([] false)
     ([x & next] `(or (not ~x) (nand ~@next)))))

#?(:cljs nil
   :default
   (defmacro nor
     "Same as (not (or ...))"
     ([] true)
     ([x & next] `(and (not ~x) (nor ~@next)))))

#?(:cljs nil
   :default
   (defmacro xor
     "Evaluates expressions one at a time, from left to right.
      If a second form evaluates to logical true, xor returns nil
      and doesn't evaluate any of the other expressions, otherwise
      it returns the value of the first logical true expression.
      If there are no truthy expressions, xor returns nil."
     ([] nil)
     ([x] `(or ~x nil))
     ([x y & next]
      `(let [x# ~x
             y# ~y]
         (when-not (and x# y#)
           (xor (or x# y#) ~@next))))))

#?(:cljs
   (defn oset
     "assoc for js objects"
     [o k v]
     (if (nil? o)
       (js-obj (name k) v)
       (do (gobj/set o (name k) v) o))))

(defn- k-name [k]
  (if (number? k) k (name k)))

#?(:cljs
   (defn- o-name [obj]
     (cond (vector? obj) "vector"
           (list? obj) "list"
           (set? obj) "set"
           (keyword? obj) "keyword"
           :else (str/lower-case (gobj/get (type obj) "name")))))

#?(:cljs
   (defn- maybe-type-error [k obj]
     (when (some #(% obj) [string? number? keyword? sequential? set?])
       (let [type-name (o-name obj)
             k         (k-name k)]
         (throw (new js/TypeError (str "Cannot create property '" k "' on " type-name " '" obj "'")))))))

#?(:cljs
   (defn oset-in
     "assoc-in for js objects"
     [o ks v]
     (if-not (seq ks)
       o
       (let [o (if (nil? o) (js-obj) o)]
         (loop [o o [k & ks] ks]
           (let [k (k-name k)]
             (if (seq ks)
               (if-let [o-next (gobj/get o k)]
                 (or (maybe-type-error (first ks) o-next)
                     (recur o-next ks))
                 (let [o-next (js-obj)]
                   (gobj/set o k o-next)
                   (recur o-next ks)))
               (gobj/set o k v))))
         o))))

#?(:cljs
   (defn oget
     "get for js objects"
     ([o k] (gobj/get o (name k) nil))
     ([o k not-found] (gobj/get o (name k) not-found))))

#?(:cljs
   (defn oget-in
     "get-in for js objects"
     ([o ks] (oget-in o ks nil))
     ([o ks not-found]
      (or (apply gobj/getValueByKeys o (map #(if (number? %) % (name %)) ks)) not-found))))

(defn new-uuid
  ^{:inline (fn [] `(random-uuid))}
  [] (random-uuid))

(defn conjv
  "ensures the seq is a vector before conj-ing"
  [col item]
  (conj (vec col) item))

(defn concatv
  "ensures the seq is a vector after concat-ing"
  [& cols]
  (vec (apply concat cols)))

(defn assocv
  "insert elem into vector at index "
  [coll i elem]
  (vec (concat (subvec coll 0 i) [elem] (subvec coll i))))

(defn dissocv
  "removes the item at index i from the vector"
  [coll i]
  (vec (concat (subvec coll 0 i) (subvec coll (inc i)))))

(defn assocv>>
  "assocv with coll as last param"
  [i elem coll]
  (assocv coll i elem))

(defn dissocv>>
  "dissocv with coll as last param"
  [i coll]
  (dissocv coll i))

(defn removev [pred col]
  "core/remove returning a vector"
  (vec (remove pred col)))

(defn removev= [col item]
  "Using =, returns vector without item"
  (removev #(= % item) col))

(defn ffilter
  "Same as (first (filter ...)), but faster!"
  [pred coll]
  (reduce (fn [_ b] (when (pred b) (reduced b))) nil coll))

(defn sum-by
  "Sums the results of a function applied to the elements of a collection"
  [f coll]
  (transduce (map f) + coll))

(defn map-some
  "DEPRECATED: Use `keep` instead.
   Like (filter some? (map f coll)).
   Returns a transducer when no collection is provided."
  ([f] (comp (map f) (filter some?)))
  ([f coll] (sequence (map-some f) coll))
  ([f coll & colls] (apply sequence (map-some f) coll colls)))

(defn some-map
  "Like (map f (filter some? coll)).
   Returns a transducer when no collection is provided."
  ([f] (comp (filter some?) (map f)))
  ([f coll] (sequence (some-map f) coll)))

(defn map-set
  "Like (set (map f coll))"
  [f coll]
  (into #{} (map f) coll))

(defn mapcat-set
  "Like (set (mapcat f coll))"
  [f coll]
  (into #{} (mapcat f) coll))

(defn map-distinct
  "Like (distinct (map f coll))"
  ([f] (comp (map f) (distinct)))
  ([f coll] (sequence (map-distinct f) coll)))

(defn rsort
  "Same as sort, but reversed"
  ([coll] (rsort compare coll))
  ([comp coll] (sort (fn [x y] (comp y x)) coll)))

(defn rsort-by
  "Same as sort-by, but reversed"
  ([keyfn coll] (rsort-by keyfn compare coll))
  ([keyfn comp coll] (sort-by keyfn (fn [x y] (comp y x)) coll)))

(defn drop-until
  "Returns a lazy sequence of the items in coll starting from the
   first item for which (pred item) returns logical true.  Returns a
   stateful transducer when no collection is provided."
  ([pred] (drop-while (complement pred)))
  ([pred coll] (drop-while (complement pred) coll)))

(defn take-until
  "Returns a lazy sequence of successive items from coll while
   (pred item) returns logical false. pred must be free of side effects.
   Returns a transducer when no collection is provided."
  ([pred] (take-while (complement pred)))
  ([pred coll] (take-while (complement pred) coll)))

(defn- greatest-v [greater? init vals]
  (reduce #(if (greater? (compare %1 %2)) %1 %2) init vals))

(defn max-v
  "Like max, but works with non-numeric values"
  ([] nil)
  ([init & vals] (greatest-v pos? init vals)))

(defn min-v
  "Like min, but works with non-numeric values"
  ([] nil)
  ([init & vals] (greatest-v neg? init vals)))

(defn- greatest-m [greater? k comp x y]
  (if (greater? (comp (get x k) (get y k))) x y))

(defn- greatest-by
  ([pos-neg? k coll] (greatest-by pos-neg? k compare coll))
  ([pos-neg? k comp [first & rest]]
   (reduce #(greatest-m pos-neg? k comp %1 %2) first rest)))

(defn max-by
  "Like max-key, but works with non-numeric values."
  ([k coll] (greatest-by pos? k coll))
  ([k comp coll] (greatest-by pos? k comp coll)))

(defn min-by
  "Like min-key, but works with non-numeric values."
  ([k coll] (greatest-by neg? k coll))
  ([k comp coll] (greatest-by neg? k comp coll)))

(defn ->inspect
  "Insert in threading macro to print the value."
  [v]
  (prn "->inspect: " v)
  v)

(defn index-of
  "Returns the index of e (using =) in the seq. nil if missing."
  [e coll]
  (first (keep-indexed #(when (= e %2) %1) coll)))

(defn ->options
  "Takes keyword argument and converts them to a map.  If the args are prefixed with a map, the rest of the
  args are merged in."
  [options]
  (cond
    (nil? options) {}
    (= [nil] options) {}
    (map? (first options)) (merge (first options) (apply hash-map (rest options)))
    :else (apply hash-map options)))

(defn formats
  "Platform-agnostic string format function."
  [format & args]
  #?(:clj  (apply clojure.core/format format args)
     :cljd (.call sprintf/sprintf format args)
     :cljs (apply gstring/format format args)))

(defn- ->string
  "Creates a string of a given length by repeating the provided value."
  [v length]
  (apply str (repeat length v)))

(defn pad-left
  "Pads the left side of a string.
   If no character is provided, spaces will be used.
   If the string is greater than or equal to the pad length,
   the unmodified string will be returned."
  ([s length] (pad-left s length " "))
  ([s length v]
   (let [size (count s)]
     (cond->>
       s
       (> length size)
       (str (->string v (- length size)))))))

(defn pad-right
  "Pads the right side of a string.
   If no character is provided, spaces will be used.
   If the string is greater than or equal to the pad length,
   the unmodified string will be returned."
  ([s length] (pad-right s length " "))
  ([s length v]
   (let [size (count s)]
     (cond->
       s
       (> length size)
       (str (->string v (- length size)))))))

(defn pad-left!
  "Same as pad-left, except the string will be trimmed to the desired length."
  ([s length] (pad-left! s length " "))
  ([s length v]
   (let [size (count s)]
     (cond
       (= length size) s
       (> size length) (subs s (- size length))
       :else (str (->string v (- length size)) s)))))

(defn pad-right!
  "Same as pad-right, except the string will be trimmed to the desired length."
  ([s length] (pad-right! s length " "))
  ([s length v]
   (let [size (count s)]
     (cond
       (= length size) s
       (> size length) (subs s 0 length)
       :else (str s (->string v (- length size)))))))

(defn char-code-at
  "Char code at the given index of a string"
  [^String s i]
  #?(:clj  (int (.charAt s i))
     :cljd (.codeUnitAt s i)
     :cljs (.charCodeAt s i)))

(defn first-char-code
  "Char code at index 0 of a string"
  [s] (char-code-at s 0))

(defn not-blank? [s] (not (str/blank? s)))

(defn remove-blanks
  "Return a map where all the keys with blank values are removed"
  [e]
  (reduce (fn [r [k v]] (if (str/blank? (str v)) r (assoc r k v))) {} e))

(defn remove-nils
  "Return a map where all the keys with nil values are removed"
  [e]
  (reduce (fn [r [k v]] (if (nil? v) r (assoc r k v))) {} e))

(defn ex?
  "Returns true is e is an exception/error for the running platform"
  [e]
  #?(:cljs    (instance? js/Error e)
     :default (instance? Exception e)))

(defn noop
  "Does nothing"
  [& _])

(defn invoke
  {:doc            "Insert in threading macro to invoke a function with a given set of arguments."
   :inline         (fn [f & args] `(~f ~@args))
   :inline-arities pos?}
  [f & args] (apply f args))

(defn narity
  "Create an n-arity function from a 0-arity function"
  [f]
  (fn [& _] (f)))

(defn find-by
  "Filters coll by items matching kvs."
  [coll & {:as kvs}]
  (cond->> coll
           (seq kvs)
           (filter (fabric/spec->tester kvs))))

(defn ffind-by
  "Finds the first item in coll matching kvs."
  [coll & {:as kvs}] (ffilter (fabric/spec->tester kvs) coll))

(defn count-where
  "Counts the number of items in coll that satisfy a predicate"
  [pred coll]
  (reduce #(cond-> %1 (pred %2) inc) 0 coll))

(defn count-by
  "Counts the number of items in coll that
   exactly match some given key-value pairs"
  [coll & {:as kvs}]
  (if (seq kvs)
    (count-where (fabric/spec->tester kvs) coll)
    (count coll)))
