(ns c3kit.apron.test-helper
  (:require [clojure.test :refer [is]]))

; speclj -> clojure.test expansions

(defn float= [a b delta]
  (<= (- a delta) b (+ a delta)))

(defn contained-in? [coll thing]
  (if (or (set? coll) (map? coll))
    (contains? coll thing)
    (some #(= thing %) coll)))

#?(:clj (defmacro should=
          ([expected actual] `(is (= ~expected ~actual)))
          ([expected actual delta] `(is (float= ~expected ~actual ~delta)))))

#?(:clj (defmacro should [result]
          `(is ~result)))

#?(:clj (defmacro should-not [result]
          `(is (not ~result))))

#?(:clj (defmacro should-contain [thing coll]
          `(is (contained-in? ~coll ~thing))))

#?(:clj (defmacro should-not-contain [thing coll]
          `(is (not (contained-in? ~coll ~thing)))))

#?(:clj (defmacro should-be-nil [thing]
          `(is (nil? ~thing))))
