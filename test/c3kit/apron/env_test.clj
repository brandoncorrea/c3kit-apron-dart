(ns c3kit.apron.env-test
  (:require [c3kit.apron.env :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import (java.io ByteArrayInputStream)))

(deftest env

  (testing "read .env content"

    (testing "missing"
      (is (= {} (sut/-read-properties "a-file-that-certainly-does-not-exist"))))

    (testing "empty"
      (with-redefs [io/reader (constantly (io/reader (ByteArrayInputStream. (.getBytes ""))))]
        (is (= {} (sut/-read-properties ".env")))))

    (testing "values"
      (let [content "foo=bar\nFIZZ=BANG"]
        (with-redefs [io/reader (constantly (io/reader (ByteArrayInputStream. (.getBytes content))))]
          (let [result (sut/-read-properties ".env")]
            (is (= {"foo" "bar" "FIZZ" "BANG"} result))))))
    )

  (with-redefs [sut/-locals       (delay {"PATH" "local"})
                sut/-sys-env      (constantly nil)
                sut/-sys-property (constantly nil)]
    (testing "env"

      (testing "from .env"
        (reset! sut/-overrides {})
        (is (= "local" (sut/env "PATH"))))

      (testing "from ENV"
        (reset! sut/-overrides {})
        (with-redefs [sut/-sys-env (constantly "sys-env")]
          (is (= "sys-env" (sut/env "PATH")))))

      (testing "from System properties"
        (reset! sut/-overrides {})
        (with-redefs [sut/-sys-env      (constantly "sys-env")
                      sut/-sys-property (constantly "sys-prop")]
          (is (= "sys-prop" (sut/env "PATH")))))

      (testing "from override"
        (reset! sut/-overrides {})
        (with-redefs [sut/-sys-env      (constantly "sys-env")
                      sut/-sys-property (constantly "sys-prop")]
          (sut/override! "PATH" "override")
          (is (= "override" (sut/env "PATH")))))

      (testing "multiple keys"
        (reset! sut/-overrides {})
        (sut/override! "FOO" "bar")
        (is (= "bar" (sut/env "FOO" "FIZZ")))
        (is (= "bar" (sut/env "FIZZ" "FOO"))))

      ))

  (with-redefs [sut/-locals       (delay {"PATH" "local"})
                sut/-sys-env      (constantly nil)
                sut/-sys-property (constantly nil)]
    (testing "env!"

      (testing "existing"
        (reset! sut/-overrides {})
        (is (= "local" (sut/env! "PATH")))
        (is (= "local" (sut/env! "FOO" "PATH"))))

      (testing "missing"
        (reset! sut/-overrides {})
        (is (thrown? Exception (sut/env! "FOO")))
        (is (thrown? Exception (sut/env! "FOO" "BAR"))))

      (testing "blank"
        (reset! sut/-overrides {})
        (sut/override! "FOO" "")
        (is (thrown? Exception (sut/env! "FOO"))))

      )
    )
  )