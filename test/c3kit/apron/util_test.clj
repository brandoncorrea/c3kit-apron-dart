(ns c3kit.apron.util-test
  (:require [c3kit.apron.log :as log]
            [c3kit.apron.util :as sut]
            [clojure.test :refer [deftest is testing]])
  (:import (java.io ByteArrayInputStream)))

(def foo "Foo")

(deftest util

  (testing "resolve-var"
    (is (thrown? Exception (deref (sut/resolve-var 'foo/bar))))
    (is (= "Foo" (deref (sut/resolve-var 'c3kit.apron.util-test/foo)))))

  (testing "path->namespace"
    (is (= "foo" (sut/path->namespace "foo.clj")))
    (is (= "foo" (sut/path->namespace "foo.clj")))
    (is (= "hello.world" (sut/path->namespace "hello/world.clj")))
    (is (= "hello.cljwhatever" (sut/path->namespace "hello/cljwhatever.clj")))
    (is (= "acme.foo.src.clj.hello" (sut/path->namespace "acme/foo/src/clj/hello.clj")))
    (is (= "foo.bar.fizz-bang" (sut/path->namespace "foo/bar/fizz_bang")))
    (is (= "fizz-bang" (sut/path->namespace "fizz_bang.clj")))
    (is (= "foo.bar.fizz-bang" (sut/path->namespace "foo/bar/fizz_bang.clj"))))

  (testing "var-value"

    (testing "nil"
      (log/capture-logs
        (is (nil? (sut/var-value nil)))
        (is (= "" (log/captured-logs-str)))))

    (testing "missing ns"
      (log/capture-logs
        (is (nil? (sut/var-value 'foo/bar)))
        (is (= "Unable to resolve var: foo/bar java.io.FileNotFoundException: Could not locate foo__init.class, foo.clj or foo.cljc on classpath."
               (log/captured-logs-str)))))

    (testing "missing var"
      (log/capture-logs
        (is (nil? (sut/var-value 'c3kit.apron.util-test/bar)))
        (is (= "Unable to resolve var: c3kit.apron.util-test/bar java.lang.Exception: No such var c3kit.apron.util-test/bar"
               (log/captured-logs-str)))))

    (testing "success"
      (log/capture-logs
        (is (= "Foo" (sut/var-value 'c3kit.apron.util-test/foo)))
        (is (= "" (log/captured-logs-str)))))

    )

  (testing "config value"

    (testing "nil"
      (is (nil? (sut/config-value nil))))

    (testing "value"
      (is (= :foo (sut/config-value :foo))))

    (testing "sym"
      (is (= "Foo" (sut/config-value 'c3kit.apron.util-test/foo))))

    )

  (testing "md5"
    (is (= "8622b9718771d75e07734684d6efa1dd" (sut/md5 "I'm a little teapot"))))

  (testing "stream->md5"
    (is (= "8622b9718771d75e07734684d6efa1dd"
           (sut/stream->md5 (ByteArrayInputStream. (.getBytes "I'm a little teapot" "UTF-8"))))))


  (testing "resources-in"

    (testing "namespace->path"
      (is (= "foo/bar/fizz_bang" (sut/namespace->path "foo.bar.fizz-bang"))))

    (testing "file system"
      (let [result (set (sut/resources-in "c3kit.apron"))]
        (is (contains? result "app.clj"))
        (is (contains? result "util.clj"))
        (is (contains? result "log.cljc"))))

    (testing "jar file"
      (let [result (set (sut/resources-in "clojure.java"))]
        (is (contains? result "io"))
        (is (contains? result "io.clj"))
        (is (contains? result "shell.clj"))))

    (testing "missing"
      (is (nil? (sut/resources-in "some.missing.package"))))

    )
  )
