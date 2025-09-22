(ns c3kit.apron.env
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io FileNotFoundException)))

(defn -read-properties [readable]
  (let [props (java.util.Properties.)]
    (try
      (.load props (io/reader readable))
      (catch FileNotFoundException _))
    props))


(def -overrides (atom {}))
(def -locals (delay (-read-properties ".env")))
(defn -sys-env [key] (System/getenv key))
(defn -sys-property [key] (System/getProperty key))

(defn override!
  "Override the value of any key that may exist in the environment."
  [key value]
  (swap! -overrides assoc key value))

(defn env
  "Resolves the key to the first matching value with the following priority:
    1) overridden values
    2) Java System properties
    3) Environment variables
    4) '.env' file (Java Properties format)"
  ([key]
   (or (get @-overrides key)
      (-sys-property key)
      (-sys-env key)
      (get @-locals key)))
  ([key & keys]
   (->> (cons key keys)
        (map env)
        (filter some?)
        first)))

(defn env!
  "Like env, but throws an exception if the key (or keys) resolved to nil or blank string."
  ([key]
   (let [result (env key)]
     (if (str/blank? result)
       (throw (ex-info (str "unresolved env " key) {}))
       result)))
  ([key & keys]
   (let [result (apply env key keys)]
     (if (str/blank? result)
       (throw (ex-info (str "unresolved env " key keys) {}))
       result))))