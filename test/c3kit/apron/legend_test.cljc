(ns c3kit.apron.legend-test
  (:require [clojure.test :refer [deftest testing is]]
            [c3kit.apron.schema :as schema]
            [c3kit.apron.legend :as sut]))

(def foo
  {:kind  (schema/kind :foo)
   :id    schema/id
   :name  {:type :string}
   :value {:type :int}})

(def bar
  {:kind   (schema/kind :bar)
   :shape  {:type :string}
   :colors {:type [:keyword]}})

(def legend (sut/build [foo bar]))

(deftest legend-test

  (testing "schemas are normalized"
    (is (= {:type :seq :spec {:type :keyword}}) (get-in legend [:bar :colors])))

  (testing "init!"
    (sut/init! legend)
    (is (= #{:foo :bar} (set (keys sut/index))))
    (is (= foo (:foo sut/index)))
    (is (= (schema/normalize-schema bar) (:bar sut/index))))

  (testing "presents an entity contained"
    (let [bob          {:kind :foo :name "Bob"}
          presentation (sut/present! bob)]
      (is (= (schema/present! foo bob) presentation))))

  (testing "complains when the entity kind is not in the legend"
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs cljs.core/ExceptionInfo
                    :cljd cljd.core/ExceptionInfo)
                 (sut/present! {:kind :unknown}))))

  (testing "coerces an entity contained"
    (let [bob      {:kind :foo :name "Bob"}
          coercion (sut/coerce! bob)]
      (is (= (schema/coerce! foo bob) coercion))))

  (testing "conforms an entity contained"
    (let [bob          {:kind :foo :name "Bob"}
          conformation (sut/conform! bob)]
      (is (= (schema/conform! foo bob) conformation))))

  )