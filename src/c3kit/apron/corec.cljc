(ns c3kit.apron.corec
  "Common core code.  This file should have minimal dependencies.
  Clients should be able to safely :refer :all from this namespace."
  #?(:clj (:import (java.util UUID))))

(defn conjv
  "ensures the seq is a vector before conj-ing"
  [col item]
  (conj (vec col) item))

(defn removev [pred col]
  "core/remove returning a vector"
  (vec (remove pred col)))

(defn map-set
  "Like (set (map f coll))"
  [f coll]
  (into #{} (map f) coll))

(defn new-uuid []
  #?(:clj  (UUID/randomUUID)
     :cljs (random-uuid)))