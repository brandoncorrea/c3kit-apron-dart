(ns c3kit.apron.bench-test
  (:require [c3kit.apron.bench :as sut]
            [c3kit.apron.corec :as ccc]
            [clojure.test :refer [deftest is use-fixtures]]))

(def invocations (atom 0))
(defn body-fn [] (swap! invocations inc))

(use-fixtures
  :each
  (fn [& args]
    (reset! invocations 0)
    (some-> (first args) ccc/invoke)))

(deftest bench-0-iterations
  (is (nil? (sut/bench 0 (body-fn))))
  (is (zero? @invocations)))

(deftest bench-negative-1-iteration
  (is (nil? (sut/bench -1 (body-fn))))
  (is (zero? @invocations)))

(deftest bench-1-iteration
  (is (sut/bench 1 (body-fn)))
  (is (= 1 @invocations)))

(deftest bench-missing-body
  (is (nil? (sut/bench 1000)))
  (is (zero? @invocations)))

(deftest bench-many-items-in-body
  (is (sut/bench 1
                 (body-fn)
                 (+ 1 2)
                 (body-fn)))
  (is (= 2 @invocations)))

(deftest executes-body-10-times
  (let [{:keys [min max avg total]} (sut/bench 10 (body-fn))]
    (is (double? min))
    (is (double? max))
    (is (double? avg))
    (is (double? total))
    (is (not (neg? min)))
    (is (not (neg? max)))
    (is (not (neg? avg)))
    (is (not (neg? total))))
  (is (= 10 @invocations)))

#?(:cljd nil
   :default
   (deftest bench-increasing-timings
     (let [timings (atom (range))]
       (with-redefs [sut/-millis-since (fn [_]
                                         (let [v (first @timings)]
                                           (swap! timings rest)
                                           v))]
         (let [{:keys [min max total avg]} (sut/bench 5 (body-fn))]
           (is (= 0 min))
           (is (= 4 max))
           (is (= 2 avg))
           (is (= 10 total)))))))
