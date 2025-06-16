(ns c3kit.apron.cursor-test
  (:require [c3kit.apron.cursor :refer [cursor]]
            [clojure.test :refer [deftest is testing]]))

(def base (atom {:a {:b {:c 0}}}))
(def a (cursor base [:a]))
(def b (cursor base [:a :b]))
(def c (cursor base [:a :b :c]))

(deftest cursor-test

  (testing "not an atom"
    (is (thrown? #?(:clj Throwable :cljs js/Error :cljd Error) (cursor nil [:a])))
    (is (thrown? #?(:clj Throwable :cljs js/Error :cljd Error) (cursor 0 [:a])))
    (is (thrown? #?(:clj Throwable :cljs js/Error :cljd Error) (cursor "" [:a]))))

  (testing "pulling"
    (is (= 0 (deref c)))
    (is (= 0 @c))
    (is (= {:c 0} @b))
    (is (= {:b {:c 0}} @a))
    (is (nil? (deref (cursor a [:blah]))))
    (is (= @base (deref (cursor base [])))))

  (testing "swapping"
    (swap! c inc)
    (is (= 1 @c))
    (is (= {:c 1} @b))
    (is (= {:b {:c 1}} @a))
    (is (= {:a {:b {:c 1}}} @base)))

  (testing "resetting"
    (reset! c 3)
    (is (= 3 @c))
    (is (= {:c 3} @b))
    (is (= {:b {:c 3}} @a))
    (is (= {:a {:b {:c 3}}} @base)))

  #?(:clj
     (testing "swap-vals!"
       (reset! c 0)
       (let [[old new] (swap-vals! c inc)]
         (is (zero? old))
         (is (= 1 new))
         (is (= 1 @c)))))

  #?(:clj
     (testing "reset-vals!"
       (reset! c 0)
       (let [[old new] (reset-vals! c 8)]
         (is (zero? old))
         (is (= 8 new))
         (is (= 8 @c)))))

  (testing "equality"
    (is (true? (= c c)))
    (is (false? (= c (cursor base [:a :b :c]))))
    (is (false? (= c b))))

  (testing "printing"
    (reset! c 0)
    (is (= "#<Cursor: 0 @[:a :b :c]>" (pr-str c))))

  (testing "watching"
    (let [change (atom nil)
          a      (atom {:b 0})
          b      (cursor a [:b])]
      (add-watch b :test (fn [k r o n] (reset! change [k r o n])))
      (swap! b inc)
      (is (= [:test b 0 1] @change))))

  (testing "stop watching"
    (let [change (atom nil)
          a      (atom {:b 0})
          b      (cursor a [:b])]
      (add-watch b :test (fn [k r o n] (reset! change [k r o n])))
      (remove-watch b :test)
      (swap! b inc)
      (is (nil? @change))))

  (testing "multiple watchers"
    (let [change (atom nil)
          a      (atom {:b 0})
          b      (cursor a [:b])]
      (add-watch b :test1 (fn [k r o n] (swap! change conj [k r o n])))
      (add-watch b :test2 (fn [k r o n] (swap! change conj [k r o n])))
      (swap! b inc)
      (is (= 2 (count @change)))
      (is (some #{[:test1 b 0 1]} @change))
      (is (some #{[:test2 b 0 1]} @change))))
  )
