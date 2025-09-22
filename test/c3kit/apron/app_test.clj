(ns c3kit.apron.app-test
  (:require [c3kit.apron.app :as app]
            [clojure.test :refer [deftest is testing]]))

(defonce bar (app/resolution :bar))
(defonce bar! (app/resolution! :bar))

(deftest app

  (testing "resolution - missing"
    (alter-var-root #'app/app dissoc :bar)
    (is (nil? @bar)))

  (testing "resolution - found"
    (alter-var-root #'app/app assoc :bar "bar")
    (is (= "bar" @bar)))

  (testing "resolution! - missing"
    (alter-var-root #'app/app dissoc :bar)
    (is (thrown? Exception #"Unresolved app component: :bar" @bar!)))

  (testing "resolution! - found"
    (alter-var-root #'app/app assoc :bar "bar")
    (is (= "bar" @bar!)))

  (testing "find-env"
    (let [token (str (rand))]
      (is (= "development" (app/find-env (str "c3.app.spec." token) (str "C3_APP_SPEC_" token)))))

    (System/setProperty "c3.app.spec" "test")
    (is (= "test" (app/find-env "c3.app.spec" "C3_APP_SPEC"))))

  (testing "env"
    (app/set-env! "env-test")
    (is (= "env-test" @app/env)))

  )
