(ns c3kit.apron.legend
  (:require [c3kit.apron.schema :as schema]))

(def retract {:kind (schema/kind :db/retract) :id schema/id})

#?(:cljd    (def ^:mutable index {})
   :default (def ^:dynamic index {}))

(defn build [schemas]
  (->> (flatten schemas)
       (filter :kind)
       (map schema/conform-schema!)
       (reduce #(assoc %1 (-> %2 :kind :value) %2) {})))

(defn init!
  [schemas]
  #?(:clj     (alter-var-root #'index (fn [_] schemas))
     :default (set! index schemas)))

(defn for-kind
  ([kind] (for-kind index kind))
  ([index kind]
   (or (get index kind)
       (throw (ex-info (str "Missing legend for kind: " (pr-str kind)) {:kind kind})))))

(defn present!
  ([entity] (present! index entity))
  ([index entity]
   (when entity
     (schema/present! (for-kind index (:kind entity)) entity))))

(defn coerce!
  ([entity] (coerce! index entity))
  ([index entity]
   (when entity
     (schema/coerce! (for-kind index (:kind entity)) entity))))

(defn conform!
  ([entity] (conform! index entity))
  ([index entity]
   (when entity
     (schema/conform! (for-kind index (:kind entity)) entity))))
