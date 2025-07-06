(ns c3kit.apron.time
  (:require
    [clojure.math :as math]
    #?(:cljs [cljs-time.format :as timef])
    #?(:cljs [cljs-time.coerce :as timec])
    #?(:cljs [cljs-time.core :as time])
    #?(:cljd ["package:intl/intl.dart" :as intl]))
  #?@(:cljd ()
      :clj  ((:import
               [java.util Date Calendar GregorianCalendar TimeZone]
               [java.text SimpleDateFormat]))))

(defn milliseconds
  ^{:doc    "Our atomic unit"
    :inline (fn [n] n)}
  [n] n)

(defn seconds
  "Converts seconds to milliseconds"
  [n] (math/round (double (* n 1000))))

(defn minutes
  "Converts minutes to milliseconds"
  [n] (math/round (double (* n 60000))))

(defn hours
  "Converts hours to milliseconds"
  [n] (math/round (double (* n 3600000))))

(defn days
  "Converts days to milliseconds"
  [n] [:days (if (float? n) (math/round n) n)])

(defn months
  "Converts a number into a format that the Calendar object understands to be an amount of months"
  [n] [:months (if (float? n) (math/round n) n)])

(defn years
  "Converts a number into a format that the Calendar object understands to be an amount of years"
  [n] [:years (if (float? n) (math/round n) n)])

(defn millis->seconds
  "Converts milliseconds to seconds"
  [millis]
  (long (/ millis 1000)))

(defn now
  ^{:doc    "Returns a java.util.Date or js/Date object that represents the current date and time in UTC"
    :inline (fn []
              #?(:clj  `(Date.)
                 :cljs `(js/Date.)
                 :cljd `(DateTime/now)))}
  []
  #?(:clj  (Date.)
     :cljs (js/Date.)
     :cljd (DateTime/now)))

(defn utc-offset
  "The offset (milliseconds) between the local timezone and UTC. (AZ -> -7hrs)"
  ([] (utc-offset (now)))
  (#?(:cljd    [^DateTime date]
      :default [date])
    #?(:clj  (.getOffset (TimeZone/getDefault) (.getTime date))
       :cljs (* -1 (minutes (.getTimezoneOffset date)))
       :cljd (.-inMilliseconds (.-timeZoneOffset date)))))

(defn from-epoch
  "Create Date relative to epoch, adjusted for timezone offset
  (from-epoch 0)"
  #?(:cljd    [^int millis-since-epoch]
     :default [^long millis-since-epoch])
  #?(:clj  (Date. millis-since-epoch)
     :cljs (js/Date. millis-since-epoch)
     :cljd (DateTime/fromMillisecondsSinceEpoch millis-since-epoch .isUtc true)))

(def epoch (from-epoch 0))

(defn instant? [thing]
  (instance? #?(:clj Date :cljs js/Date :cljd DateTime) thing))

(defn millis-since-epoch
  #?(:cljd    [^DateTime date]
     :default [date])
  #?(:cljd    (.-millisecondsSinceEpoch date)
     :default (.getTime date)))

(defn seconds-since-epoch [time]
  (-> time millis-since-epoch millis->seconds))

(defn millis-between
  "Milliseconds that separate the two times.  Negative if b is after a."
  [a b]
  (- (millis-since-epoch a) (millis-since-epoch b)))

(defn ->utc
  "Returns a new date representing time in UTC timezone, assuming given date is in local timezone."
  #?(:cljd    [^DateTime date]
     :default [^Date date])
  (from-epoch (- (millis-since-epoch date) (utc-offset date))))

(defn ->local
  "Returns a new date representing time in the timezone, assuming given date is in UTC timezone."
  #?(:cljd    [^DateTime date]
     :default [^Date date])
  (from-epoch (+ (millis-since-epoch date) (utc-offset date))))

(defn local
  "Create a Date assuming parameters are local timezone.
  e.g. in AZ: (local 2020 1 1 0 0 0) -> 2020-01-01T07:00:00.000-00:00"
  ([year month day] (local year month day 0 0 0))
  ([year month day hour minute] (local year month day hour minute 0))
  ([year month day hour minute second]
   #?(:clj  (.getTime (GregorianCalendar. year (dec month) day hour minute second))
      :cljd (DateTime. year month day hour minute second)
      :cljs (js/Date. year (dec month) day hour minute second))))

(defn utc
  "Create a Date assuming parameters are UTC timezone.
  e.g. (utc 2020 1 1 0 0 0) -> 2020-01-01T00:00:00.000-00:00"
  ([year month day] (utc year month day 0 0 0))
  ([year month day hour minute] (utc year month day hour minute 0))
  ([year month day hour minute second] (->local (local year month day hour minute second))))

(defn before?
  "Expects two Dates as arguments. The function returns true if the
  first date comes before the second date and returns false otherwise."
  #?(:cljd    [^DateTime first ^DateTime second]
     :default [^Date first ^Date second])
  #?(:clj  (.before first second)
     :cljd (.isBefore first second)
     :cljs (< (.getTime first) (.getTime second))))


(defn after?
  "Expects two Date as arguments. The function returns true if the
  first date comes after the second date and returns false otherwise."
  #?(:cljd    [^DateTime first ^DateTime second]
     :default [^Date first ^Date second])
  #?(:clj  (.after first second)
     :cljd (.isAfter first second)
     :cljs (> (.getTime first) (.getTime second))))

(defn between?
  "Expects the three Dates as arguments. The first date is the date
  being evaluated; the second date is the start date; the last date is the
  end date. The function returns true if the first date is between the start
  and end dates."
  #?(:cljd    [^DateTime date ^DateTime start ^DateTime end]
     :default [^Date date ^Date start ^Date end])
  (and
    (after? date start)
    (before? date end)))

(defn leap-year? [year]
  (or (and (zero? (mod year 4))
           (not (zero? (mod year 100))))
      (zero? (mod year 400))))

(defn days-in-month [year month]
  (case month
    0 31
    1 (if (leap-year? year) 29 28)
    2 31
    3 30
    4 31
    5 30
    6 31
    7 31
    8 30
    9 31
    10 30
    11 31))

#?(:clj (defn to-calendar
          "Converts a Date object into a GregorianCalendar object"
          [datetime]
          (doto (GregorianCalendar.)
            (.setTime datetime))))

(defn year
  "Returns the Date's year (local timezone)."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  #?(:clj  (.get (to-calendar datetime) Calendar/YEAR)
     :cljd (.-year datetime)
     :cljs (.getFullYear datetime)))

(defn month
  "Returns the Date's month (local timezone)."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  (inc #?(:clj  (.get (to-calendar datetime) Calendar/MONTH)
          :cljd (dec (.-month datetime))
          :cljs (.getMonth datetime))))

(defn day
  "Returns the Date's day (local timezone)."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  #?(:clj  (.get (to-calendar datetime) Calendar/DAY_OF_MONTH)
     :cljd (.-day datetime)
     :cljs (.getDate datetime)))

(defn hour
  "Returns the Date's hour (24-hour clock) (local timezone)."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  #?(:clj  (.get (to-calendar datetime) Calendar/HOUR_OF_DAY)
     :cljd (.-hour datetime)
     :cljs (.getHours datetime)))

(defn minute
  "Returns the Date's minute."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  #?(:clj  (.get (to-calendar datetime) Calendar/MINUTE)
     :cljd (.-minute datetime)
     :cljs (.getMinutes datetime)))

(defn sec
  "Returns the Date's second."
  #?(:cljd    [^DateTime datetime]
     :default [^Date datetime])
  #?(:clj  (.get (to-calendar datetime) Calendar/SECOND)
     :cljd (.-second datetime)
     :cljs (.getSeconds datetime)))

#?(:cljs (defmulti -js-mod-time-by-units (fn [_time unit _n] unit)))
#?(:cljs (defmethod -js-mod-time-by-units :days [time _unit n] (.setDate time (+ (.getDate time) n))))
#?(:cljs (defmethod -js-mod-time-by-units :months [time _unit n]
           (let [date (.getUTCDate time)]
             (.setUTCDate time 1)
             (.setUTCMonth time (+ (.getUTCMonth time) n))
             (let [month    (.getUTCMonth time)
                   max-date (days-in-month (.getUTCFullYear time) month)]
               (.setUTCDate time (min date max-date))))))
#?(:cljs (defmethod -js-mod-time-by-units :years [time _unit n]
           (let [day   (.getUTCDate time)
                 month (.getUTCMonth time)
                 year  (+ (.getUTCFullYear time) n)
                 day   (min day (days-in-month year month))]
             (doto time
               (.setUTCDate day)
               (.setFullYear year)))))

(def ^:private microseconds-per-day (* 24 60 60 1000 1000))

#?(:cljd
   (defn- -cljd-mod-time [^DateTime time unit n]
     (case unit
       :days (-> (.-microsecondsSinceEpoch time)
                 (+ (* n microseconds-per-day))
                 (DateTime/fromMicrosecondsSinceEpoch))
       :months (let [months (+ (.-month time) n)
                     year   (+ (.-year time) (quot months 13))
                     month  (rem months 13)
                     [year month] (if (pos? month)
                                    [year month]
                                    [(dec year) (+ 12 month)])
                     day    (min (.-day time) (days-in-month year (dec month)))]
                 (DateTime. year month day
                            (.-hour time)
                            (.-minute time)
                            (.-second time)
                            (.-millisecond time)
                            (.-microsecond time)))
       :years (let [year  (+ (.-year time) n)
                    month (.-month time)
                    day   (min (.-day time) (days-in-month year (dec month)))]
                (DateTime. year month day
                           (.-hour time)
                           (.-minute time)
                           (.-second time)
                           (.-millisecond time)
                           (.-microsecond time)))
       (throw (ex-info (str "invalid duration unit: " unit) {:unit unit})))))

(defn- mod-time-by-units
  "Modifies the value of a Date object. Expects the first argument to be
  a Date, the second argument to be a vector representing the amount of time to be changed,
  and the last argument to be either a + or - (indicating which direction to modify time)."
  [time [unit n] direction]
  (let [n (direction n)]
    #?(:clj  (let [calendar (GregorianCalendar.)
                   unit     (case unit
                              :days Calendar/DATE
                              :months Calendar/MONTH
                              :years Calendar/YEAR
                              (throw (ex-info (str "invalid duration unit: " unit) {:unit unit})))]
               (.setTime calendar time)
               (.add calendar unit (int n))
               (.getTime calendar))
       :cljd (-cljd-mod-time time unit n)
       :cljs (doto (js/Date. (.getTime time))
               (-js-mod-time-by-units unit n)))))

(defn- mod-time
  "Modifies the value of a Date. Expects the first argument to be
  a Date object, the second argument to be an amount of milliseconds, and
  the last argument to be either a + or - (indicating which direction to
  modify time)."
  #?(:cljd    [^DateTime time bit direction]
     :default [time bit direction])
  (cond
    (number? bit) #?(:clj  (Date. (direction (.getTime time) (long bit)))
                     :cljd (let [micros             (* 1000 (long bit))
                                 micros-since-epoch (direction (.-microsecondsSinceEpoch time) micros)]
                             (DateTime/fromMicrosecondsSinceEpoch micros-since-epoch))
                     :cljs (js/Date. (direction (.getTime time) (long bit))))
    (vector? bit) (mod-time-by-units time bit direction)))

(defn before
  "Rewinds the time on a Date object. Expects a Date object as the first
  argument and a number of milliseconds to rewind time by."
  [time & bits]
  (reduce #(mod-time %1 %2 -) time bits))

(defn after
  "Fast-forwards the time on a Date object. Expects a Date object as the first
  argument and a number of milliseconds to fast-forward time by."
  [time & bits]
  (reduce #(mod-time %1 %2 +) time bits))

(def earlier? before?)
(def later? after?)
(def earlier before)
(def later after)

(defn ago
  "Returns a Date some time (n) before now."
  [n]
  (before (now) n))

(defn from-now
  "Returns a Date some time (n) after now."
  [n]
  (after (now) n))

(defprotocol IFormatter
  (-parse [formatter value])
  (-unparse [formatter value]))

#?(:clj (extend-type SimpleDateFormat
          IFormatter
          (-parse [formatter value]
            (.parse formatter value))
          (-unparse [formatter value]
            (.format formatter value))))

#?(:cljs
   (extend-type timef/Formatter
     IFormatter
     (-parse [formatter value]
       (let [goog-dt (timef/parse formatter value)]
         (timec/to-date goog-dt)))
     (-unparse [formatter value]
       (let [goog-dt (timec/from-date value)]
         (timef/unparse formatter goog-dt)))))

#?(:cljd
   (extend-type intl/DateFormat
     IFormatter
     (-parse [formatter value]
       (.parseUtc formatter value))
     (-unparse [formatter value]
       (.format formatter value))))

(defn formatter [format]
  #?(:clj  (let [sdf (SimpleDateFormat. format)]
             (.setTimeZone sdf (TimeZone/getTimeZone "UTC"))
             sdf)
     :cljd (intl/DateFormat. format)
     :cljs (timef/formatter format)))

;#?(:cljd
;   (deftype ISO8601Formatter [^intl/DateFormat unparse-formatter]
;     IFormatter
;     (-parse [_formatter value]
;       (DateTime/parse value))
;     (-unparse [_formatter value]
;       (.format unparse-formatter value))))

#?(:cljd
   (defn- ->ISO8601Formatter [unparse-format]
     (let [^intl/DateFormat formatter (formatter unparse-format)]
       (reify IFormatter
         (-parse [_ value] (DateTime/parse value))
         (-unparse [_ value] (.format formatter value))))))

(def date-formats
  {
   :http       (formatter "EEE, dd MMM yyyy HH:mm:ss ZZZ")
   :rfc1123    (formatter "EEE, dd MMM yyyy HH:mm:ss ZZZ")
   :rfc822     (formatter "EEE, dd MMM yyyy HH:mm:ss Z")
   :ref3339    #?(:clj  (formatter "yyyy-MM-dd'T'HH:mm:ssXXX")
                  :cljd (->ISO8601Formatter "yyyy-MM-dd'T'HH:mm:ss'Z'")
                  :cljs (formatter "yyyy-MM-dd'T'HH:mm:ssZZ"))
   :long-no-tz (formatter "EEE, dd MMM yyyy HH:mm:ss")
   :iso8601    #?(:cljd    (->ISO8601Formatter "yyyy-MM-dd HH:mm:ss'Z'")
                  :default (formatter "yyyy-MM-dd HH:mm:ssZ"))
   :dense      (formatter "yyyyMMddHHmmss")
   :ymd        (formatter "yyyyMMdd")
   :webform    (formatter "yyyy-MM-dd")
   :web-local  (formatter "yyyy-MM-dd'T'HH:mm")
   :friendly   (formatter "EEE - MMM d, yyyy")
   :short      (formatter "MMM d, yyyy")
   })

(defn- ->formatter [format]
  (cond
    (keyword? format) (format date-formats)
    (string? format) (formatter format)
    #?@(:clj [(instance? SimpleDateFormat format) format])
    #?@(:cljs [(instance? timef/Formatter format) format])
    #?@(:cljd [(instance? intl/DateFormat format) format])
    :else (throw (ex-info (str "Unhandled date format: " format) {:format format}))))

(defn parse
  "Parses text into a Java Date object. Expects a keyword, string, or SimpleDateFormat
  object as the first object and a string representing the date as the second argument.
  The date is assumed to be in UTC."
  [format value]
  (let [formatter (->formatter format)]
    (-parse formatter value)))

(defn unparse
  "Returns a string that is populated with a formatted date and time. Expects the
  first argument to be the requested format and the second argument to be the date
  to be formatted.
  The following are options for the first argument:
  1. Keyword - :http, :rfc1123, :iso8601, :dense
  2. String - must be a valid argument to the SimpleDateFormat Java Object
  3. SimpleDateFormat - Java Object"
  [format value]
  (when value
    (let [formatter (->formatter format)]
      (-unparse formatter value))))

(defn bounds
  ^{:inline (fn [start end] `(list ~start ~end))}
  [start end] (list start end))

(defn bounds? [thing]
  (and (seq? thing)
       (= 2 (count thing))
       (instant? (first thing))
       (instant? (first (rest thing)))))

(defn start-of
  ^{:inline (fn [bounds] `(first ~bounds))}
  [bounds] (first bounds))

(defn end-of
  ^{:inline (fn [bounds] `(second ~bounds))}
  [bounds] (second bounds))

(defn during? [bounds instant]
  (and (after? instant (start-of bounds))
       (before? instant (end-of bounds))))
