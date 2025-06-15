(ns c3kit.apron.time-test
  (:require
    [clojure.test :refer [testing is deftest]]
    [c3kit.apron.time :as sut :refer [now local before? after? between? year month day hour minute sec
                                      parse unparse years months days hours minutes seconds before after
                                      ago from-now formatter ->utc utc-offset utc millis-since-epoch
                                      earlier? later? earlier later]])
  #?@(:cljd ()
      :clj  ((:import [java.util Date]
                      [java.text SimpleDateFormat]))))

(deftest time-test

  (testing "now"
    (let [now (now)]
      #?(:clj  (is (= Date (.getClass now)))
         :cljs (is (true? (instance? js/Date now)))
         :cljd (is (true? (instance? DateTime now))))
      #?(:clj  (is (> 100 (- (System/currentTimeMillis) (.getTime now))))
         :cljs (is (> 100 (- (. (js/Date.) (getTime)) (.getTime now))))
         :cljd (is (> 100 (- (.-millisecondsSinceEpoch (DateTime/now)) (.-millisecondsSinceEpoch ^DateTime now)))))))

  (testing "millis->seconds"
    (is (= 0 (sut/millis->seconds 0)))
    (is (= 0 (sut/millis->seconds 999)))
    (is (= 1 (sut/millis->seconds 1000)))
    (is (= 1 (sut/millis->seconds 1500)))
    (is (= 15 (sut/millis->seconds 15000))))

  (testing "seconds-since-epoch"
    (let [now     (now)
          seconds (long (/ (sut/millis-since-epoch now) 1000))]
      (is (= seconds (sut/seconds-since-epoch now)))))

  (testing "creating Dates and getting the pieces"
    (let [dt (local 2011 1 1)]
      (is (= 2011 (year dt)))
      (is (= 1 (month dt)))
      (is (= 1 (day dt)))
      (is (= 0 (hour dt)))
      (is (= 0 (minute dt)))
      (is (= 0 (sec dt))))
    (let [dt (local 2011 1 2 3 4)]
      (is (= 2011 (year dt)))
      (is (= 1 (month dt)))
      (is (= 2 (day dt)))
      (is (= 3 (hour dt)))
      (is (= 4 (minute dt)))
      (is (= 0 (sec dt))))
    (let [dt (local 2012 3 4 5 6 7)]
      (is (= 2012 (year dt)))
      (is (= 3 (month dt)))
      (is (= 4 (day dt)))
      (is (= 5 (hour dt)))
      (is (= 6 (minute dt)))
      (is (= 7 (sec dt)))))

  (testing "creating from epoch"
    ;January 1, 1970, 00:00:00 GMT.
    (let [epoch (->utc (sut/from-epoch 0))]
      (is (= 1970 (year epoch)))
      (is (= 1 (month epoch)))
      (is (= 1 (day epoch)))
      (is (= 0 (hour epoch)))
      (is (= 0 (minute epoch)))
      (is (= 0 (sec epoch)))))

  ; Only run in AZ timezone
  #_(it "utc offset AZ"
        (should= (* -1 (-> 7 hours)) (utc-offset))
        (should= (* -1 (-> 7 hours)) (utc-offset (now))))

  ; Only run in Central timezone
  #_(it "utc offset in Central TZ"
        (should= (* -1 (-> 5 hours)) (utc-offset (parse :dense "20221105000000")))
        (should= (* -1 (-> 5 hours)) (utc-offset (parse :dense "20221106000000")))
        (should= (* -1 (-> 6 hours)) (utc-offset (parse :dense "20221106070000")))
        (should= (* -1 (-> 6 hours)) (utc-offset (parse :dense "20221107000000"))))

  ; Only run in Central timezone
  #_(it "creates dates relative to now in day increments - across timezone"
        (let [start (local 2022 11 05)]                     ;; 1 day1 before DSL begins
          (should= (parse :dense "20221105050000") start)
          (should= (parse :dense "20221106050000") (after start (-> 1 days)))
          (should= (parse :dense "20221107060000") (after start (-> 2 days)))
          (should= (parse :dense "20221108060000") (after start (-> 3 days)))))

  (testing "local vs utc after DST"
    (let [local-time (local 2020 1 1 1 1 1)
          utc-time   (utc 2020 1 1 1 1 1)]
      (is (= (utc-offset local-time) (- (millis-since-epoch utc-time) (millis-since-epoch local-time))))))

  (testing "local vs utc during DST"
    (let [local-time (local 2020 6 1 1 1 1)
          utc-time   (utc 2020 6 1 1 1 1)]
      (is (= (utc-offset local-time) (- (millis-since-epoch utc-time) (millis-since-epoch local-time))))))

  (testing "before? and after?"
    (is (true? (before? (local 2011 1 1) (local 2011 1 2))))
    (is (false? (before? (local 2011 1 3) (local 2011 1 2))))
    (is (false? (after? (local 2011 1 1) (local 2011 1 2))))
    (is (true? (after? (local 2011 1 3) (local 2011 1 2)))))

  (testing "checks if a date is between two other dates"
    (is (true? (between? (local 2011 1 2) (local 2011 1 1) (local 2011 1 3))))
    (is (false? (between? (local 2011 1 3) (local 2011 1 2) (local 2011 1 1)))))

  (testing "creates dates relative to now in second increments"
    (is (true? (before? (-> 1 seconds ago) (now))))
    (is (true? (before? (-> 2 seconds ago) (-> 1 seconds ago))))
    (is (true? (after? (-> 1 seconds from-now) (now))))
    (is (true? (after? (-> 2 seconds from-now) (-> 1 seconds from-now))))
    (is (true? (after? (-> 0.5 seconds from-now) (now)))))

  (testing "creates dates relative to now in minute increments"
    (is (true? (before? (-> 1 minutes ago) (now))))
    (is (true? (before? (-> 1 minutes ago) (-> 59 seconds ago))))
    (is (false? (before? (-> 1 minutes ago) (-> 61 seconds ago))))
    (is (true? (after? (-> 1 minutes from-now) (now))))
    (is (true? (after? (-> 1 minutes from-now) (-> 59 seconds from-now))))
    (is (false? (after? (-> 1 minutes from-now) (-> 61 seconds from-now))))
    (is (true? (after? (-> 0.5 minutes from-now) (now)))))

  (testing "creates dates relative to now in hour increments"
    (is (true? (before? (-> 1 hours ago) (now))))
    (is (true? (before? (-> 1 hours ago) (-> 59 minutes ago))))
    (is (false? (before? (-> 1 hours ago) (-> 61 minutes ago))))
    (is (true? (after? (-> 1 hours from-now) (now))))
    (is (true? (after? (-> 1 hours from-now) (-> 59 minutes from-now))))
    (is (false? (after? (-> 1 hours from-now) (-> 61 minutes from-now))))
    (is (true? (after? (-> 0.5 hours from-now) (now)))))

  (testing "creates dates relative to now in day increments"
    (is (true? (before? (-> 1 days ago) (now))))
    (is (before? (-> 1 days ago) (-> 23 hours ago)))
    (is (false? (before? (-> 1 days ago) (-> 25 hours ago))))
    (is (true? (after? (-> 1 days from-now) (now))))
    (is (true? (after? (-> 1 days from-now) (-> 23 hours from-now))))
    (is (false? (after? (-> 1 days from-now) (-> 25 hours from-now))))
    (is (true? (after? (-> 0.5 days from-now) (now)))))

  (testing "create dates relative to other dates by month increment"
    (is (= "20110201" (unparse :ymd (after (local 2011 1 1) (months 1)))))
    (is (= "20101201" (unparse :ymd (before (local 2011 1 1) (months 1)))))
    (is (true? (after? (-> 1 months from-now) (-> 27 days from-now))))
    (is (false? (after? (-> 1 months from-now) (-> 32 days from-now))))
    (is (true? (before? (-> 1 months ago) (-> 27 days ago))))
    (is (false? (before? (-> 1 months ago) (-> 32 days ago)))))

  (testing "earlier later aliases"
    (is (= before? earlier?))
    (is (= after? later?))
    (is (= before earlier))
    (is (= after later)))

  (testing "leap-year?"
    (is (false? (sut/leap-year? 2011)))
    (is (true? (sut/leap-year? 2012)))
    (is (false? (sut/leap-year? 2100)))
    (is (true? (sut/leap-year? 2400))))

  (testing "days in month"
    (is (= 31 (sut/days-in-month 2000 0)))
    (is (= 29 (sut/days-in-month 2000 1)))
    (is (= 28 (sut/days-in-month 2001 1)))
    (is (= 31 (sut/days-in-month 2001 2)))
    (is (= 30 (sut/days-in-month 2001 3)))
    (is (= 31 (sut/days-in-month 2001 4)))
    (is (= 30 (sut/days-in-month 2001 5)))
    (is (= 31 (sut/days-in-month 2001 6)))
    (is (= 31 (sut/days-in-month 2001 7)))
    (is (= 30 (sut/days-in-month 2001 8)))
    (is (= 31 (sut/days-in-month 2001 9)))
    (is (= 30 (sut/days-in-month 2001 10)))
    (is (= 31 (sut/days-in-month 2001 11))))

  (testing "rolling over a month with not enough days"
    (is (= "20110228" (unparse :ymd (after (local 2011 1 31) (months 1)))))
    (is (= "20250228" (unparse :ymd (after (local 2024 2 29) (years 1))))))

  (testing "create dates relative to other dates by year increment"
    (is (= "20120101" (unparse :ymd (after (local 2011 1 1) (years 1)))))
    (is (= "20100101" (unparse :ymd (before (local 2011 1 1) (years 1)))))
    (is (true? (after? (-> 1 years from-now) (-> 11 months from-now))))
    (is (false? (after? (-> 1 years from-now) (-> 13 months from-now))))
    (is (true? (before? (-> 1 years ago) (-> 11 months ago))))
    (is (false? (before? (-> 1 years ago) (-> 13 months ago)))))

  (testing "month and year units are rounded"
    (is (= [:months 1] (months 0.5)))
    (is (= [:months 0] (months 0.4)))
    (is (= [:years 1] (years 0.5)))
    (is (= [:years 0] (years 0.4))))

  (testing "parses and formats dates in HTTP format"
    (let [date (parse :http "Sun, 06 Nov 1994 08:49:37 GMT")]
      (is (true? (after? date (local 1994 11 5))))
      (is (true? (before? date (local 1994 11 7))))
      ;      (should= "Sun, 06 Nov 1994 02:49:37 -0600" (unparse :http date)) ; only works in certain CST zone
      ))

  (testing "parses and formats dates in custom format"
    (let [date (parse "MMM d, yyyy HH:mm" "Nov 6, 1994 08:49")]
      (is (= "Nov 6, 1994 08:49" (unparse "MMM d, yyyy HH:mm" date)))))

  (testing "parses and formats dates in custom format object"
    (let [format (formatter "MMM d, yyyy HH:mm")
          date   (parse format "Nov 6, 1994 08:49")]
      (is (= "Nov 6, 1994 08:49" (unparse format date)))))

  (testing "parses and formats dates in ISO 8601 format"
    #?(:clj  (let [date (parse :iso8601 "1994-11-06 08:49:12 GMT")]
               (is (= "1994-11-06 08:49:12+0000" (unparse :iso8601 date))))
       :cljs (let [date (parse :iso8601 "1994-11-06 08:49:12Z")]
               (is (= "1994-11-06 08:49:12Z" (unparse :iso8601 date))))
       :cljd (let [date (parse :iso8601 "1994-11-06 08:49:12Z")]
               (is (= "1994-11-06 08:49:12Z" (unparse :iso8601 date))))))

  (testing "parses REF 3339 format"
    (let [date1 (parse :ref3339 "2022-03-04T23:59:02-05:00")
          date2 (parse :ref3339 "2022-03-04T23:59:02+05:00")
          date3 (parse :ref3339 "2022-03-04T23:59:02-00:00")
          date4 (parse :ref3339 "2022-03-04T23:59:02Z")]
      (is (= "2022-03-05T04:59:02Z" (unparse :ref3339 date1)))
      (is (= "2022-03-04T18:59:02Z" (unparse :ref3339 date2)))
      (is (= "2022-03-04T23:59:02Z" (unparse :ref3339 date3)))
      (is (= "2022-03-04T23:59:02Z" (unparse :ref3339 date4)))))

  (testing "parses and formats :webform datas"
    (let [date (parse :webform "2020-03-31")
          utc  (sut/->utc date)]
      (is (= 2020 (year utc)))
      (is (= 3 (month utc)))
      (is (= 31 (day utc)))
      (is (= 0 (hour utc)))
      (is (= 0 (minute utc)))
      (is (= 0 (sec utc)))
      (is (= "2020-03-31" (unparse :webform date)))))

  (testing "parses and formats :web-local datas"
    (let [date (parse :web-local "2024-02-29T01:23")
          utc  (sut/->utc date)]
      (is (= 2024 (year utc)))
      (is (= 2 (month utc)))
      (is (= 29 (day utc)))
      (is (= 1 (hour utc)))
      (is (= 23 (minute utc)))
      (is (= 0 (sec utc)))
      (is (= "2024-02-29T01:23" (unparse :web-local date)))))

  (testing "time range"
    (let [time1 (sut/local 1939 9 1)
          time2 (sut/local 1945 9 2)
          ww2   (sut/bounds time1 time2)]
      (is (sut/bounds? ww2))
      (is (= time1 (sut/start-of ww2)))
      (is (= time2 (sut/end-of ww2)))
      (is (not (sut/during? ww2 (sut/local 1920 1 1))))
      (is (sut/during? ww2 (sut/local 1941 1 1)))
      (is (not (sut/during? ww2 (sut/local 1950 1 1))))))

  )
