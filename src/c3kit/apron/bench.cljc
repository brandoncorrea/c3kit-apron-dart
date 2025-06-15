(ns c3kit.apron.bench
  #?(:cljs (:require-macros [c3kit.apron.bench :refer [elapsed-time bench]])))

(defn -millis-now []
  #?(:clj  (/ (System/nanoTime) 1000000.0)
     :cljd (doto (Stopwatch.)
             (.start))
     :cljs (js-invoke js/performance "now")))

(defn -millis-since [millis-start]
  #?(:cljd    (/ (.-elapsedMicroseconds ^Stopwatch millis-start) 1000.0)
     :default (- (-millis-now) millis-start)))

(defmacro elapsed-time
  "Evaluates the body and returns the elapsed time in milliseconds."
  [& body]
  `(let [start# (-millis-now)]
     (do ~@body)
     (-millis-since start#)))

(defmacro bench
  "Test how long a body will take to execute.
   This will only `(do body)`. If you need to realize
   all results in a sequence, wrap your body in a `doall`.
   Body is executed n times."
  [n & body]
  (when (seq body)
    `(let [n# ~n]
       (when (pos? n#)
         (let [timings# (repeatedly n# #(elapsed-time ~@body))
               total#   (apply + timings#)]
           {:min   (apply min timings#)
            :max   (apply max timings#)
            :total total#
            :avg   (/ total# n#)})))))
