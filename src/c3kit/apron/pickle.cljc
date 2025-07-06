(ns c3kit.apron.pickle
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [c3kit.apron.utilc :as utilc]))

;(def LONG_STRING_LENGTH 16)

(defprotocol Pickleable
  "An object that can be pickled."
  (pickleable-type [_]
    "Return a unique type. Can be anything, but keyword is convenient.
    map->pickleable must be implemented for ths same type.")
  (pickleable->map [_] "Convert the object to a map."))

(defmulti map->pickleable
          "Convert a map into a object, dispatched off the pickleable-type."
          (fn [_t _m] _t))

#?(:cljs (def js-uid-counter (atom 0)))

(defn -unique-id [o]
  #?(:clj  (System/identityHashCode o)
     :cljd (dart:core/identityHashCode o)
     :cljs (if-let [id (ccc/oget o "_pickle_id")]
             id
             (let [id (swap! js-uid-counter inc)]
               (ccc/oset o "_pickle_id" id)
               id))))

(defn- ref-value [id] {:_t :ref :_v id})

(declare ref! do-pickle)

(defn- pickled-map-value [refs m]
  (into {} (map (fn [[k v]] [(do-pickle refs k) (do-pickle refs v)]) m)))

(defn- pickle-map [refs o]
  (let [v (pickled-map-value refs o)]
    {:_t :map :_v v}))

(defn- pickle-seq [refs o]
  {:_t :seq :_v (mapv (partial do-pickle refs) o)})

(defn- pickle-set [refs o]
  {:_t :set :_v (mapv (partial do-pickle refs) o)})

(defn- pickle-instant [o]
  {:_t :inst :_v (pr-str o)})

(defn- pickle-custom [refs o]
  {:_t (pickleable-type o) :_v (pickled-map-value refs (pickleable->map o))})

(defn- ref! [refs o]
  (let [id (-unique-id o)]
    (when-not (contains? @refs id)
      (let [pickled (cond
                      (record? o) (pickle-custom refs o)
                      (satisfies? Pickleable o) (pickle-custom refs o)
                      (sequential? o) (pickle-seq refs o)
                      (map? o) (pickle-map refs o)
                      (set? o) (pickle-set refs o)
                      (time/instant? o) (pickle-instant o)
                      :else o)]
        (swap! refs assoc id pickled)))
    (ref-value id)))

(defn- scalar? [o]
  (not (or (sequential? o)
           (map? o)
           (record? o)
           (satisfies? Pickleable o)
           (set? o)
           (time/instant? o))))

(defn- do-pickle [refs obj]
  (if (scalar? obj) obj (ref! refs obj)))

(defn pickle [obj]
  (let [refs   (atom {})
        result (do-pickle refs obj)]
    (if (empty? @refs)
      result
      {:_refs @refs :_object result})))

(declare do-unpickle)

(defn- unpickle-ref [refs id]
  (let [pickled   (get refs id)
        unpickled (do-unpickle refs pickled)]
    ;; TODO - MDM: cache?
    unpickled))

(defn- unpickle-map [refs o]
  (into {} (map (fn [[k v]] [(do-unpickle refs k) (do-unpickle refs v)]) o)))

(defn- unpickle-seq [refs o]
  (mapv (partial do-unpickle refs) o))

(defn- unpickle-set [refs o]
  (set (map (partial do-unpickle refs) o)))

(defn- unpickle-instant [o] (utilc/<-edn o))

(defn- unpickle-custom [refs _t _v]
  (when-not (map? _v) (throw (ex-info (str "unpickle-custom value is not a map: " (pr-str _v)) {:_t _t :_v _v})))
  (let [m (unpickle-map refs _v)]
    (map->pickleable _t m)))

(defn- do-unpickle [refs obj]
  (if (map? obj)
    (let [_t (:_t obj)
          _v (:_v obj)]
      (case _t
        nil (throw (ex-info (str "unpickle: missing type" obj) {:obj obj}))
        :ref (unpickle-ref refs _v)
        :set (unpickle-set refs _v)
        :map (unpickle-map refs _v)
        :seq (unpickle-seq refs _v)
        :inst (unpickle-instant _v)
        (unpickle-custom refs _t _v)))
    obj))

(defn unpickle [pickled-obj]
  (if (and (map? pickled-obj) (contains? pickled-obj :_refs))
    (do-unpickle (:_refs pickled-obj) (:_object pickled-obj))
    pickled-obj))
