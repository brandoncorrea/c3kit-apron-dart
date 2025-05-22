(ns c3kit.apron.utilc-test
  (:require [clojure.test :refer [deftest is]]
            [c3kit.apron.utilc :as sut]))

(deftest edn
  (is (= "[1 2 3]" (sut/->edn [1 2 3])))
  (is (= [1 2 3] (sut/<-edn "[1 2 3]"))))
