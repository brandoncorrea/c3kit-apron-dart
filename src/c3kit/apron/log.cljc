(ns c3kit.apron.log
  (:refer-clojure :exclude [time])
  #?(:cljs (:require-macros [c3kit.apron.log :refer [trace debug info warn error fatal report capture-logs]]))
  (:require [c3kit.apron.corec :as ccc]
            [clojure.string :as str]
            #?@(:cljd    ()
                :default ([taoensso.timbre :as timbre]))))

;; MDM - If you're not seeing a log entry, check your Chrome Dev Tools console levels.

; For accurate line numbers in Chrome, add these Blackbox patterns:
; /taoensso/timbre/appenders/core\.js$
; /taoensso/timbre\.js$
; /cljs/core\.js$
;
; See: https://goo.gl/ZejSvR

(def captured-logs (atom []))

(defn capture-log!
  "Arity overrides for timbre/-log!"
  ([config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data]
   (swap! captured-logs conj [config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data])
   nil)
  ([config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data callsite-id]
   (swap! captured-logs conj [config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data callsite-id])
   nil)
  ([config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data callsite-id spying?]
   (swap! captured-logs conj [config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data callsite-id spying?])
   nil)
  ([config level ?ns-str ?file ?line ?column msg-type ?err vargs_ ?base-data callsite-id spying?]
   (swap! captured-logs conj [config level ?ns-str ?file ?line ?column msg-type ?err vargs_ ?base-data callsite-id spying?])
   nil))

#?(:cljd
   (defn ^:dynamic -log!
     ([_config _level _?ns-str _?file _?line _msg-type _?err vargs_ _?base-data]
      (apply println @vargs_))
     ([_config _level _?ns-str _?file _?line _msg-type _?err vargs_ _?base-data _callsite-id]
      (apply println @vargs_))
     ([_config _level _?ns-str _?file _?line _msg-type _?err vargs_ _?base-data _callsite-id _spying?]
      (apply println @vargs_))
     ([_config _level _?ns-str _?file _?line _?column _msg-type _?err vargs_ _?base-data _callsite-id _spying?]
      (apply println @vargs_)))
   :default
   (def -log! timbre/-log!))

#?(:cljd (def min-level (atom :debug)))

(defn set-min-level! [level]
  #?(:cljd    (reset! min-level level)
     :default (timbre/set-min-level! level)))

(defn level []
  #?(:cljd    @min-level
     :default (:min-level timbre/*config*)))

#?(:cljd (defmacro -do-log [level & args]
           `(-log! nil ~level nil nil nil nil :p nil (delay (list ~@args)) nil nil false)))

(defmacro trace [& args]
  #?(:cljd    `(-do-log :trace ~@args)
     :default `(timbre/trace ~@args)))

(defmacro debug [& args]
  #?(:cljd    `(-do-log :debug ~@args)
     :default `(timbre/debug ~@args)))

(defmacro info [& args]
  #?(:cljd    `(-do-log :info ~@args)
     :default `(timbre/info ~@args)))

(defmacro warn [& args]
  #?(:cljd    `(-do-log :warn ~@args)
     :default `(timbre/warn ~@args)))

(defmacro error [& args]
  #?(:cljd    `(-do-log :error ~@args)
     :default `(timbre/error ~@args)))

(defmacro fatal [& args]
  #?(:cljd    `(-do-log :fatal ~@args)
     :default `(timbre/fatal ~@args)))

(defmacro report [& args]
  #?(:cljd    `(-do-log :report ~@args)
     :default `(timbre/report ~@args)))

(defmacro capture-logs [& body]
  `(let [original-level# (level)]
     (reset! captured-logs [])
     (try
       (set-min-level! :trace)
       #?(:cljd
          (binding [-log! capture-log!]
            ~@body)
          :default
          (with-redefs [timbre/-log! capture-log!]
            ~@body))
       (finally
         (set-min-level! original-level#)))))

(defmacro with-level [level & body]
  #?(:cljd
     `(let [original-level# (level)]
        (try
          (set-min-level! ~level)
          ~@body
          (finally (set-min-level! original-level#))))
     :default `(timbre/with-min-level ~level ~@body)))

(defn test-levels [msg]
  (report msg)
  (fatal msg)
  (error msg)
  (warn msg)
  (info msg)
  (debug msg)
  (trace msg))

(defn set-level! [new-level]
  (when-not (= (level) new-level)
    (report (str "Setting log level: " new-level))
    (set-min-level! new-level)))

(defn off! [] (set-level! :report))
(defn fatal! [] (set-level! :fatal))
(defn error! [] (set-level! :error))
(defn warn! [] (set-level! :warn))
(defn info! [] (set-level! :info))
(defn debug! [] (set-level! :debug))
(defn all! [] (set-level! :trace))

(defn parse-captured-logs []
  (map
    #(hash-map :level (nth % 1) :message (apply str @(nth % 8)))
    @captured-logs))

(defn captured-logs-str []
  (->> (map #(str/join " " @(nth % 8)) @captured-logs)
       (str/join "\n")))

(defn table-spec [& cols]
  (let [width      (+ (apply + (map second cols)) (count cols))
        format-str (str/join " " (map #(str "%-" (second %) "s") cols))]
    {:cols     cols
     :format   format-str
     :width    width
     :title-fn (fn [title]
                 (let [pad (/ (- width (count title)) 2)]
                   (str (str/join "" (take pad (repeat " "))) title "\n")))
     :header   (str (apply (partial ccc/formats format-str) (map first cols)) "\n"
                    (str/join "" (take width (repeat "-"))) "\n")
     }))

(defn color-pr
  "For ANSI color codes: https://en.wikipedia.org/wiki/ANSI_escape_code"
  [message color]
  (println (str "\u001b[" color "m" message "\u001b[0m")))

(defn -platform-time []
  #?(:clj  (System/nanoTime)
     :cljs (js-invoke (js/Date.) "getTime")))

(defn -nanos []
  #?(:clj  (System/nanoTime)
     :cljd (doto (Stopwatch.) (.start))
     :cljs (* (js-invoke js/performance "now") 1000.0)))

(defn -millis-since [start]
  #?(:cljd    (/ (.-elapsedMicroseconds ^Stopwatch start) 1000.0)
     :default (/ (double (- (-nanos) start)) 1000000.0)))

(defmacro time
  "Same as clojure.core/time but logs (info) instead of printing elapsed time."
  [expr]
  `(let [start# (-nanos)
         ret#   ~expr]
     (info (str "Elapsed time: " (-millis-since start#) " msecs"))
     ret#))
