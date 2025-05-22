(ns c3kit.apron.corec-test
  (:require [clojure.test :refer [deftest is testing]]
            [c3kit.apron.corec :as ccc]))

(deftest conjv-test
  (let [result (ccc/conjv (list 1 2 3) 4)]
    (is (= [1 2 3 4] result))
    (is (vector? result))))

(deftest removev-test
  (let [result (ccc/removev even? (list 1 2 3 4))]
    (is (= [1 3] result))
    (is (vector? result))))

(deftest map-set-test
  (is (= #{} (ccc/map-set inc nil)))
  (is (= #{2} (ccc/map-set inc [1])))
  (is (= #{2} (ccc/map-set inc [1 1])))
  (is (= #{2 3 4 5} (ccc/map-set inc [1 1 2 3 4 3]))))

(deftest new-uuid-test
  (let [unique-uuids (->> (repeatedly ccc/new-uuid)
                          (take 10)
                          set)]
    (is (= 10 (count unique-uuids)))))