(ns c3kit.apron.cursor
  "Defines a Cursor that wraps an atom (or atom-like structure) and provides a way to focus on a
  specific path within its nested data. It implements various Clojure interfaces to make it behave like an atom
  itself, with dereferencing, swapping, resetting, and watching capabilities."
  #?@(:cljd ()
      :clj  ((:import (clojure.lang IDeref IAtom IRef IAtom2)))))

(defn- do-swap!
  ([base path f] (-> (swap! base update-in path f) (get-in path)))
  ([base path f x] (-> (swap! base update-in path f x) (get-in path)))
  ([base path f x y] (-> (swap! base update-in path f x y) (get-in path)))
  ([base path f x y more] (-> (swap! base update-in path (fn [v] (apply f v x y more))) (get-in path))))

(defn- swap-vals-result [path result]
  (let [[o n] result] [(get-in o path) (get-in n path)]))

(defn- do-reset! [base path new-value]
  (swap! base assoc-in path new-value)
  new-value)

(defn- to-string [this path] (str "#<Cursor: " (pr-str @this) " @" (pr-str path) ">"))

#?(:cljd
   (deftype Cursor [base path]
     cljd.core/IAtom

     cljd.core/IDeref
     (-deref [_] (get-in @base path))

     cljd.core/IReset
     (-reset! [_ new-value] (do-reset! base path new-value))

     cljd.core/ISwap
     (-swap! [a f] (do-swap! base path f))
     (-swap! [a f x] (do-swap! base path f x))
     (-swap! [a f x y] (do-swap! base path f x y))
     (-swap! [a f x y more] (do-swap! base path f x y more))

     cljd.core/IPrint
     (-print [_ sink]
       (doto ^StringSink sink
         (.write "#<Cursor: ")
         (.write (pr-str (get-in @base path)))
         (.write " @")
         (.write (pr-str path))
         (.write ">")))

     cljd.core/IWatchable
     (-notify-watches [_ oldval newval] (-notify-watches base oldval newval))
     (-add-watch [this key f] (-add-watch base [path key] (fn [k r o n] (f key this (get-in o path) (get-in n path)))))
     (-remove-watch [_ key] (-remove-watch base [path key]))
     )

   :clj
   (deftype Cursor [base path]

     IDeref
     (deref [_] (get-in @base path))

     IAtom
     (swap [_ f] (do-swap! base path f))
     (swap [_ f x] (do-swap! base path f x))
     (swap [_ f x y] (do-swap! base path f x y))
     (swap [_ f x y more] (do-swap! base path f x y more))
     (reset [_ new-value] (do-reset! base path new-value))

     IAtom2
     (swapVals [_ f] (swap-vals-result path (swap-vals! base update-in path f)))
     (swapVals [_ f x] (swap-vals-result path (swap-vals! base update-in path f x)))
     (swapVals [_ f x y] (swap-vals-result path (swap-vals! base update-in path f x y)))
     (swapVals [_ f x y more] (swap-vals-result path (swap-vals! base update-in path (fn [v] (apply f v x y more)))))
     (resetVals [_ new-value] (swap-vals-result path (swap-vals! base assoc-in path new-value)))

     Object
     (toString [this] (to-string this path))

     IRef
     (setValidator [_ v] (.setValidator base v))
     (getValidator [_] (.getValidator base))
     (getWatches [_] (.getWatches base))
     (addWatch [this key f] (.addWatch base [path key] (fn [k r o n] (f key this (get-in o path) (get-in n path)))))
     (removeWatch [_ key] (.removeWatch base [path key]))
     )

   :cljs
   (deftype Cursor [base path]
     IAtom

     IDeref
     (-deref [_] (get-in @base path))

     IReset
     (-reset! [_ new-value] (do-reset! base path new-value))

     ISwap
     (-swap! [_a f] (do-swap! base path f))
     (-swap! [_a f x] (do-swap! base path f x))
     (-swap! [_a f x y] (do-swap! base path f x y))
     (-swap! [_a f x y more] (do-swap! base path f x y more))

     IPrintWithWriter
     (-pr-writer [_ writer opts]
       (-write writer "#<Cursor: ")
       (pr-writer (get-in @base path) writer opts)
       (-write writer " @")
       (pr-writer path writer opts)
       (-write writer ">"))

     IWatchable
     (-notify-watches [_ oldval newval] (-notify-watches base oldval newval))
     (-add-watch [this key f] (-add-watch base [path key] (fn [k r o n] (f key this (get-in o path) (get-in n path)))))
     (-remove-watch [_ key] (-remove-watch base [path key]))
     )
   )

#?(:clj (defmethod clojure.core/print-method Cursor [cursor writer]
          (.write writer "#<Cursor: ")
          (.write writer (pr-str @cursor))
          (.write writer " @")
          (.write writer (pr-str (.path cursor)))
          (.write writer ">")))

(defn cursor
  "Returns a cursor that focuses on a specific path within an atom-like reference.
  The returned cursor implements deref, swap!, reset!, and watch,
  allowing it to behave like an atom scoped to the given path.

  Args:
    a    - An atom-like reference (e.g., an atom) containing nested data.
    path - A sequence of keys (e.g., [:user :name]) specifying the path to focus on.

  Example:
    (def state (atom {:user {:name \"Alice\"}}))
    (def name-cursor (cursor state [:user :name]))
    @name-cursor           ;; => \"Alice\"
    (swap! name-cursor str \" Smith\") ;; Updates state to {:user {:name \"Alice Smith\"}}

  Notes:
    - The cursor delegates operations to the base atom, modifying its state at the given path.
    - If the path becomes invalid (e.g., due to structural changes), dereferencing returns nil."
  [a path]
  (assert #?(:cljd (satisfies? cljd.core/IAtom a)
             :clj  (instance? IAtom a)
             :cljs (satisfies? IAtom a)))
  (if (seq path)
    (Cursor. a path)
    a))

