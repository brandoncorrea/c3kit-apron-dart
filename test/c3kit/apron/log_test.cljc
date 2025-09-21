(ns c3kit.apron.log-test
  #?(:cljs (:require-macros [c3kit.apron.log-test :refer [test-log-arity]]))
  (:require [c3kit.apron.log :as sut]
            [clojure.test :refer [is deftest use-fixtures]]
            [clojure.string :as str]))

(defn test-fixture
  ([])
  ([_] (with-out-str (sut/off!))))

(use-fixtures :each test-fixture)

(defmacro test-log-arity [arity]
  `(let [args# (range ~arity)]
     (is (nil? (apply sut/-log! args#)))
     (is (= args# (last @sut/captured-logs)))))

(deftest level
  (with-out-str
    (sut/debug!)
    (is (= :debug (sut/level)))
    (sut/off!)
    (is (= :report (sut/level)))
    (sut/fatal!)
    (is (= :fatal (sut/level)))
    (sut/all!)
    (is (= :trace (sut/level)))))

(deftest log-arity-overrides
  (sut/capture-logs
    (test-log-arity 9)
    (test-log-arity 10)
    (test-log-arity 11)
    ;; TODO [BAC]: Why does this fail in cljd?
    #?(:cljd nil :default (test-log-arity 12))))

(deftest capturing-logs
  (let [output (with-out-str (sut/capture-logs (sut/info "hello")))
        [_config level _?ns-str _?file _?line _?column _msg-type _?err vargs_ _?base-data _callsite-id _spying? :as _log]
        (first @sut/captured-logs)]
    (is (= "" output))
    (is (= :info level))
    (is (= ["hello"] @vargs_))
    (is (= {:level :info :message "hello"} (first (sut/parse-captured-logs))))))

(deftest logging-timing
  (sut/capture-logs
    (is (= "foo\n" (with-out-str (sut/time (println "foo"))))))
  (let [captured-logs (sut/captured-logs-str)]
    (is (str/starts-with? captured-logs "Elapsed time:"))
    (is (str/ends-with? captured-logs " msecs"))))
