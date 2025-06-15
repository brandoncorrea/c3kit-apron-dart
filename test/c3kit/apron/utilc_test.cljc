(ns c3kit.apron.utilc-test
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.utilc :as sut]
            [clojure.test :refer [deftest is testing]]
            #?@(:cljd () :default ([cognitect.transit :as transit]))))

(deftest edn-test

  (testing "->edn"
    (is (= "[1 2 3]" (sut/->edn [1 2 3]))))

  (testing "<-edn"
    (is (= [1 2 3] (sut/<-edn "[1 2 3]"))))
  )

(deftest ->hex-test
  (is (= "7b" (sut/->hex 123))))

(deftest <-hex-test
  (is (= 123 (sut/<-hex "7b")))
  (is (= 122 (sut/<-hex "7A"))))

(deftest map-manipulation-test

  (testing "keywordize kind"
    (is (= {:kind :foo :val 1} (sut/keywordize-kind {:kind "foo" :val 1})))
    (is (= {:kind :foo :val 1} (sut/keywordize-kind {:kind :foo :val 1})))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs cljs.core/ExceptionInfo
                    :cljd cljd.core/ExceptionInfo) (sut/keywordize-kind {:missing "kind"})))
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs cljs.core/ExceptionInfo
                    :cljd cljd.core/ExceptionInfo) (sut/keywordize-kind {:kind 123}))))

  (testing "index-by-id"
    (let [a {:id 123 :name "a"}
          b {:id 456 :name "b"}]
      (is (= {123 a 456 b} (sut/index-by-id [b a])))))
  )

#?(:cljd
   (deftest transit-test
     (testing "->transit not implemented"
       (is (thrown-with-msg?
             cljd.core/ExceptionInfo
             #"Not Supported in ClojureDart"
             (sut/->transit "blah")))
       (is (thrown-with-msg?
             cljd.core/ExceptionInfo
             #"Not Supported in ClojureDart"
             (sut/->transit :json {:handlers {}} "blah"))))

     (testing "<-tranasit not implemented"
       (is (thrown-with-msg?
             cljd.core/ExceptionInfo
             #"Not Supported in ClojureDart"
             (sut/<-transit "blah")))
       (is (thrown-with-msg?
             cljd.core/ExceptionInfo
             #"Not Supported in ClojureDart"
             (sut/<-transit :json {} "blah")))))
   )

(when #?(:cljd false :default true)
  (deftest transit-test

    (testing "uuid"
      (let [uuid         (ccc/new-uuid)
            uuid-transit (sut/->transit uuid)]
        (is (= uuid (sut/<-transit uuid-transit)))))

    (testing "uuid in map"
      ;#uuid "53060bf1-971a-4d18-80fc-92a3112afd6e"
      (let [uuid (sut/->uuid-or-nil "53060bf1-971a-4d18-80fc-92a3112afd6e")
            data {:uuid uuid}
            trs  (sut/->transit data)]
        (is (= data (sut/<-transit trs)))))

    (testing "<-transit accepts optional types and parameters"
      (let [handlers {"f" (transit/read-handler (fn [_] "hello"))}]
        (is (= "hello" (sut/<-transit :json {:handlers handlers} "[\"~#'\",\"~f-1.23\"]")))))

    (testing "->transit accepts optional types and parameters"
      (let [handlers {#?(:clj  clojure.lang.Symbol
                         :cljd cljd.core/Symbol
                         :cljs cljs.core/Symbol)
                      (transit/write-handler
                        (fn [_] "$")
                        (fn [_] "abc"))}]
        (is (= (sut/->transit 'abc) (sut/->transit :json {:handlers handlers} 'im-being-ignored...)))))

    (testing "BigDecimal"
      (is (= #?(:clj -1.23M :default -1.23) (sut/<-transit "[\"~#'\",\"~f-1.23\"]")))
      (is (= #?(:clj -1M :default -1) (sut/<-transit "[\"~#'\",\"~f-1\"]")))
      (is (= #?(:clj -0.00001M :default -0.00001) (sut/<-transit "[\"~#'\",\"~f-0.00001\"]")))
      (is (= #?(:clj 0M :default 0) (sut/<-transit "[\"~#'\",\"~f0\"]")))
      (is (= #?(:clj 0M :default 0) (sut/<-transit "[\"~#'\",\"~f0.0\"]")))
      (is (= #?(:clj 0.00001M :default 0.00001) (sut/<-transit "[\"~#'\",\"~f0.00001\"]")))
      (is (= #?(:clj 3M :default 3) (sut/<-transit "[\"~#'\",\"~f3\"]")))
      (is (= #?(:clj 3.14M :default 3.14) (sut/<-transit "[\"~#'\",\"~f3.14\"]")))
      (is (= {#?(:clj 3.14M :default 3.14) #?(:clj 0.15926M :default 0.15926)}
             (sut/<-transit "[\"^ \",\"~f3.14\",\"~f0.15926\"]"))))

    (testing "BigInt"
      (is (= #?(:clj -1N :default -1) (sut/<-transit "[\"~#'\",\"~n-1\"]")))
      (is (= #?(:clj 0N :default 0) (sut/<-transit "[\"~#'\",\"~n0\"]")))
      (is (= #?(:clj 5N :default 5) (sut/<-transit "[\"~#'\",\"~n5\"]")))
      (is (= {#?(:clj 3N :default 3) #?(:clj 7N :default 7)}
             (sut/<-transit "[\"^ \",\"~n3\",\"~n7\"]"))))
    ))

(deftest json-test

  (testing "->json"
    (let [json #?(:cljd "{\"a\":123,\"b\":\"hello\",\"g\":321,\"c\":[1,2,3],\"d\":{\"e\":\"f\"}}"
                  :default "{\"a\":123,\"b\":\"hello\",\"c\":[1,2,3],\"d\":{\"e\":\"f\"},\"g\":321}")]
      (is (= json (sut/->json {:a  123
                               :b  "hello"
                               :c  [1 2 3]
                               :d  {:e "f"}
                               "g" 321})))))

  (testing "->json empty values"
    (is (= "null" (sut/->json nil)))
    (is (= "\"\"" (sut/->json ""))))

  (testing "<-json"
    (is (= {"a" 123
            "b" "hello"
            "c" [1 2 3]
            "d" {"e" "f"}
            "g" 321}
           (sut/<-json "{\"a\":123,\"b\":\"hello\",\"c\":[1,2,3],\"d\":{\"e\":\"f\"},\"g\":321}"))))

  (testing "<-json empty values"
    (is (nil? (sut/<-json nil)))
    (is (nil? (sut/<-json ""))))

  (testing "<-json: keyword keys"
    (is (= {:a 123
            :b "hello"
            :c [1 2 3]
            :d {:e "f"}
            :g 321}
           (sut/<-json-kw "{\"a\":123,\"b\":\"hello\",\"c\":[1,2,3],\"d\":{\"e\":\"f\"},\"g\":321}"))))
  )

(deftest csv-test

  (testing "no rows"
    (is (= "" (sut/->csv []))))

  (testing "with rows"
    (is (= (str "A,B,C\r\n"
                "1,2,3\r\n"
                "a,b,c")
           (sut/->csv [["A" "B" "C"]
                       [1 2 3]
                       ['a 'b 'c]]))))

  (testing "with comma in value"
    (is (= (str "A,B\r\n"
                "\"a, ok\",A-OK")
           (sut/->csv [["A" "B"]
                       ["a, ok" "A-OK"]]))))

  (testing "with \" in value"
    (is (= (str "A,B\r\n"
                "\"\"\"a\"\"\",\"\"\"b\"")
           (sut/->csv [["A" "B"]
                       ["\"a\"" "\"b"]]))))
  )

(deftest ->filename-test
  (is (= "foo" (sut/->filename "foo")))
  (is (= "foo_bar" (sut/->filename "foo bar")))
  (is (= "foos_bar" (sut/->filename "foo's bar")))
  (is (= "Mr_foo" (sut/->filename "Mr. foo")))
  (is (= "foo_bar" (sut/->filename "foo-bar")))
  (is (= "foo" (sut/->filename "foo/\\<>:\"|?*[]")))
  (is (= "foo.bar" (sut/->filename "foo" "bar"))))

