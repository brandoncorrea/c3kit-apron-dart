(ns c3kit.apron.utilc
  (:require [clojure.edn :as edn]))

(defn ->edn
  "Convenience.  Convert the form to EDN"
  [v] (some-> v pr-str))

(defn <-edn
  "Convenience.  Convert the EDN string to a Clojure form"
  [s] (edn/read-string s))
