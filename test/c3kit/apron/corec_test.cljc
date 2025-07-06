(ns c3kit.apron.corec-test
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.time :as time]
            [clojure.test :refer [deftest is testing]]))

(defn lazy? [thing]
  (instance? #?(:clj  clojure.lang.LazySeq
                :cljd cljd.core/LazySeq
                :cljs cljs.core/LazySeq) thing))

(deftest nand-test
  (testing "no arguments" (is (false? (ccc/nand))))
  (testing "one falsy argument" (is (true? (ccc/nand nil))))
  (testing "one truthy argument" (is (false? (ccc/nand 1))))
  (testing "two truthy arguments" (is (false? (ccc/nand 1 2))))
  (testing "two falsy arguments" (is (true? (ccc/nand false nil))))
  (testing "three truthy arguments" (is (false? (ccc/nand 1 2 3))))
  (testing "three falsy arguments" (is (true? (ccc/nand false nil (not true)))))
  (testing "two truthy and one falsy argument" (is (true? (ccc/nand 1 2 false))))
  (testing "truthy then falsy argument" (is (true? (ccc/nand 1 nil))))
  (testing "falsy then truthy argument" (is (true? (ccc/nand nil 1))))
  (testing "lazy evaluation on the first falsy value" (is (true? (ccc/nand nil (/ 1 0)))))
  (testing "evaluates each form exactly once"
    (let [flag (atom false)]
      (is (false? (ccc/nand (swap! flag not) true)))
      (is (true? @flag)))))

(deftest nor-test
  (testing "no arguments" (is (true? (ccc/nor))))
  (testing "one falsy argument" (is (true? (ccc/nor nil))))
  (testing "one truthy argument" (is (false? (ccc/nor 1))))
  (testing "two truthy arguments" (is (false? (ccc/nor 1 2))))
  (testing "two falsy arguments" (is (true? (ccc/nor false nil))))
  (testing "three truthy arguments" (is (false? (ccc/nor 1 2 3))))
  (testing "three falsy arguments" (is (true? (ccc/nor false nil (not true)))))
  (testing "two truthy and one falsy argument" (is (false? (ccc/nor 1 2 false))))
  (testing "truthy then falsy argument" (is (false? (ccc/nor 1 nil))))
  (testing "falsy then truthy argument" (is (false? (ccc/nor nil 1))))
  (testing "lazy evaluation on the first truthy value" (is (false? (ccc/nor 1 (/ 1 0)))))
  (testing "evaluates each form exactly once"
    (let [flag (atom true)]
      (is (true? (ccc/nor (swap! flag not) nil)))
      (is (false? @flag)))))

(deftest xor-test
  (testing "no arguments" (is (nil? (ccc/xor))))
  (testing "one nil argument" (is (nil? (ccc/xor nil))))
  (testing "one false argument" (is (nil? (ccc/xor false))))
  (testing "one truthy argument" (is (= 1 (ccc/xor 1))))
  (testing "nil then false arguments" (is (nil? (ccc/xor nil false))))
  (testing "false then nil arguments" (is (nil? (ccc/xor false nil))))
  (testing "two truthy arguments" (is (nil? (ccc/xor true 1))))
  (testing "falsy then truthy arguments" (is (= 1 (ccc/xor false 1))))
  (testing "truthy then falsy arguments" (is (= 1 (ccc/xor 1 false))))
  (testing "truthy, falsy, then truthy arguments" (is (nil? (ccc/xor 1 nil 2))))
  (testing "lazy evaluation on the second truthy value" (is (nil? (ccc/xor 1 2 (/ 1 0)))))
  (testing "four arguments with one truthy value" (is (= 4 (ccc/xor nil false nil 4))))
  (testing "four arguments with two truthy values" (is (nil? (ccc/xor nil false 3 4))))
  (testing "evaluates each form exactly once"
    (let [flag (atom 0)]
      (is (= 1 (ccc/xor (swap! flag inc) nil nil)))
      (is (= 1 @flag)))))

(deftest ->options-test
  (testing "nil -> {}"
    (is (= {} (ccc/->options [nil])))))

(deftest new-uuid-test
  (is (= 10 (->> (repeatedly ccc/new-uuid)
                 (take 10)
                 distinct
                 count))))

(deftest conjv-test
  (let [result (ccc/conjv (list 1 2 3) 4)]
    (is (= [1 2 3 4] result))
    (is (true? (vector? result)))))

(deftest concatv-test
  (let [result (ccc/concatv (list 1 2 3) (list 4))]
    (is (= [1 2 3 4] result))
    (is (true? (vector? result)))))

(deftest dissocv-test
  (let [result (ccc/dissocv [1 2 3] 1)]
    (is (= [1 3] result))
    (is (vector? result))))

(deftest assocv-test
  (let [result (ccc/assocv [1 2 3] 1 :foo)]
    (is (= [1 :foo 2 3] result))
    (is (vector? result))))

(deftest removev-test
  (let [result (ccc/removev even? (list 1 2 3 4))]
    (is (= [1 3] result))
    (is (vector? result))))

(deftest removev=-test
  (let [result (ccc/removev= (list 1 2 3 4) 2)]
    (is (= [1 3 4] result))
    (is (vector? result))))

(deftest map-all-test
  (is (= [2 3 4] (ccc/map-all inc [1 2 3])))
  (is (= [5 7 9] (ccc/map-all + [1 2 3] [4 5 6])))
  (is (= [12 15 18] (ccc/map-all + [1 2 3] [4 5 6] [7 8 9]))))

#?(:cljs
   (deftest map-component-test
     (let [[one two three] (ccc/map-component inc ccc/noop [1 2 3])]
       (is (vector? one))
       (is (vector? two))
       (is (vector? three))
       (is (= [ccc/noop 1] one))
       (is (= [ccc/noop 2] two))
       (is (= [ccc/noop 3] three))
       (is (= {:key 2} (meta one)))
       (is (= {:key 3} (meta two)))
       (is (= {:key 4} (meta three))))))


(deftest ffilter-test
  (for [coll [nil [] [nil]]]
    (testing (str "is nil when " (pr-str coll))
      (is (nil? (ccc/ffilter any? coll)))))
  (testing "single-element collection"
    (is (= 1 (ccc/ffilter any? [1]))))
  (testing "two-element collection"
    (is (= 2 (ccc/ffilter any? [2 1]))))
  (testing "first item of filtered result"
    (is (= :a (ccc/ffilter identity [nil false :a :b]))))
  (testing "first item of no results"
    (is (nil? (ccc/ffilter number? [nil false :a :b])))))

(deftest count-where-test
  (is (= 0 (ccc/count-where pos? [])))
  (is (= 0 (ccc/count-where nil? [])))
  (is (= 0 (ccc/count-where nil? [1])))
  (is (= 0 (ccc/count-where nil? [1 2])))
  (is (= 0 (ccc/count-where nil? [1 2])))
  (is (= 2 (ccc/count-where pos? [1 2])))
  (is (= 1 (ccc/count-where odd? [1 2])))
  (is (= 5 (ccc/count-where some? [1 2 nil 3 4 5]))))

(deftest count-by-test
  (let [e1     {:foo "bar" :size 2 :round? false}
        e2     {:foo "bar" :size 3 :round? true}
        e3     {:bar "foo" :size 2 :round? nil :hello :world}
        e4     {:bar "foo" :size 2 :hello :world}
        things [e1 e2 e3 e4]]
    (is (= 0 (ccc/count-by [] :foo "bar")))
    (is (= 2 (ccc/count-by things :foo "bar")))
    (is (= 2 (ccc/count-by things :bar "foo")))
    (is (= 0 (ccc/count-by things :foo "foo")))
    (is (= 0 (ccc/count-by things :foo "foo" :size 2)))
    (is (= 2 (ccc/count-by things {:bar "foo" :size 2})))
    (is (= 1 (ccc/count-by things :round? false)))
    (is (= 1 (ccc/count-by things :round? true)))
    (is (= 2 (ccc/count-by things :round? nil)))
    (is (= 2 (ccc/count-by things :round? ['not nil])))
    (is (= 0 (ccc/count-by things :round? :blah)))))

(deftest find-by-test
  (let [e1    {:name "hello"}
        e2    {:name "world" :size 1 :pets [nil]}
        e3    {:name "hello world" :size 2 :pets ["dog"]}
        e4    {:name "hi!" :size 2 :pets ["dog" "cat"]}
        items [e1 e2 e3 e4]]

    (testing "map options"
      (is (= [e4] (ccc/find-by items {:size ['> 1] :name "hi!"}))))

    (testing "greater than or less than"
      (is (= [] (ccc/find-by items :size ['> 2])))
      (is (= [e3 e4] (ccc/find-by items :size ['> 1])))
      (is (= [e2 e3 e4] (ccc/find-by items :size ['>= 1])))
      (is (= [] (ccc/find-by items :size ['< 1])))
      (is (= [e2] (ccc/find-by items :size ['<= 1])))
      (is (= [] (ccc/find-by items :name ['> "world"])))
      (is (= [] (ccc/find-by items :name ['< "hello"])))
      (is (= [e1] (ccc/find-by items :name ['<= "hello"])))
      (is (= [e2] (ccc/find-by items :name ['>= "world"]))))

    (testing "equal and not equal"
      (is (= [e2] (ccc/find-by items :size ['= 1])))
      (is (= [e3 e4] (ccc/find-by items :size ['= 2])))
      (is (= [] (ccc/find-by items :size ['= 0])))
      (is (= [e1] (ccc/find-by items :size ['= nil])))
      (is (= [e1 e2 e3 e4] (ccc/find-by items :size ['not= 0])))
      (is (= [e1 e3 e4] (ccc/find-by items :size ['not= 1])))
      (is (= [e1 e2] (ccc/find-by items :size ['not= 2])))
      (is (= [e1] (ccc/find-by items :size ['not= 2 1])))
      (is (= [e2 e3 e4] (ccc/find-by items :size ['not= nil]))))

    (testing "in"
      (is (= [e1 e2] (ccc/find-by items :size [1 nil]))))

    (testing "like fuzzy match with anything before or after"
      (is (= [e2 e3] (ccc/find-by items :name ['like "%orl%"])))
      (is (= [e2] (ccc/find-by items :name ['like "worl%"])))
      (is (= [e1] (ccc/find-by items :name ['like "%ello"]))))

    (testing "like fuzzy match with _"
      (let [e5    {:name "words"}
            items (conj items e5)]
        (is (= [e2 e5] (ccc/find-by items :name ['like "wor__"])))))

    (testing "like with exact match"
      (is (= [e1] (ccc/find-by items :name ['like "hello"])))
      (is (= [e2] (ccc/find-by items :name ['like "world"]))))

    (testing "or"
      (is (= [e2 e3 e4] (ccc/find-by items :size [1 2])))
      (is (= [e1 e2] (ccc/find-by items :size [1 nil])))
      (is (= [e2] (ccc/find-by items :size [1]))))

    (testing "finds first"
      (is (= e3 (ccc/ffind-by items :size 2)))
      (is (= e1 (ccc/ffind-by items :size nil))))

    (testing "in seq"
      (is (= [] (ccc/find-by items :pets "blah")))
      (is (= [e4] (ccc/find-by items :pets "cat")))
      (is (= [e3 e4] (ccc/find-by items :pets "dog")))
      (is (= [e3 e4] (ccc/find-by items :pets ['like "%o%"])))
      (is (= [e4] (ccc/find-by items :pets ['like "%a%"]))))
    ))

(deftest sum-by-test
  (let [e1     {:size 2}
        e2     {:size 3}
        e3     {:size -3}
        e4     {:size 2}
        things [e1 e2 e3 e4]]
    (is (= 0 (ccc/sum-by :blah [])))
    (is (= 0 (ccc/sum-by :blah nil)))
    (is (= 2 (ccc/sum-by :size [e1])))
    (is (= -1 (ccc/sum-by :size [e1 e3])))
    (is (= 4 (ccc/sum-by :size things)))))

(deftest map-some-test
  (testing "removes nil values from mapping"
    (is (= [] (ccc/map-some identity nil)))
    (is (= [] (ccc/map-some identity [])))
    (is (= [1] (ccc/map-some identity [1])))
    (is (= [1 2 3] (ccc/map-some identity [1 2 3])))
    (is (= [false true false] (ccc/map-some even? [1 2 3])))
    (is (= [1 3] (ccc/map-some :a [{:a 1} {:b 2} {:a 3 :b 4}])))
    (is (= [2 4] (ccc/map-some :b [{:a 1} {:b 2} {:a 3 :b 4}])))
    (is (= [] (ccc/map-some :c [{:a 1} {:b 2} {:a 3 :b 4}]))))

  (testing "is lazy"
    (is (lazy? (ccc/map-some identity [1 2 3])))
    (is (lazy? (ccc/map-some identity [1])))
    (let [empty (ccc/map-some identity [])]
      #?(:clj  (is (= (class clojure.lang.PersistentList/EMPTY) (type empty)))
         :cljd (is (= [] empty))
         :cljs (is (lazy? empty)))))

  (testing "accepts multiple collections"
    (is (= [[1] [2] [3]] (ccc/map-some vector [1 2 3])))
    (is (= [[1 4] [2 5] [3 6]] (ccc/map-some vector [1 2 3] [4 5 6])))
    (is (= [[1 4 7] [2 5 8] [3 6 9]] (ccc/map-some vector [1 2 3] [4 5 6] [7 8 9])))
    (letfn [(maybe-even [x y z] (let [sum (+ x y z)] (when (even? sum) sum)))]
      (is (= [12 18] (ccc/map-some maybe-even [1 2 3] [4 5 6] [7 8 9])))))

  (testing "creates a transducer"
    (is (= [1 2 3] (transduce (ccc/map-some identity) conj [1 2 3])))
    (is (= [2 4] (eduction (ccc/map-some :a) (map inc) [{:a 1} {:b 2} {:a 3 :b 4}])))
    (is (= [2 4] (sequence (comp (ccc/map-some :a) (map inc)) [{:a 1} {:b 2} {:a 3 :b 4}])))
    (is (= [] (transduce (ccc/map-some identity) conj [])))
    (is (= [1 3] (transduce (ccc/map-some identity) conj [1 nil 3])))))

(deftest some-map-test
  (testing "removes nil values before mapping is applied"
    (is (= [] (ccc/some-map identity nil)))
    (is (= [] (ccc/some-map identity [])))
    (is (= [1] (ccc/some-map identity [1])))
    (is (= [] (ccc/some-map identity [nil])))
    (is (= [1 2 3] (ccc/some-map identity [1 2 3])))
    (is (= [false true] (ccc/some-map even? [1 nil 2])))
    (is (= [4 2 11] (ccc/some-map inc [3 nil 1 10 nil])))
    (is (= [nil 2 4] (ccc/some-map :b [{:a 1} {:b 2} {:a 3 :b 4}])))
    (is (= [nil nil nil] (ccc/some-map :c [{:a 1} {:b 2} {:a 3 :b 4}]))))

  (testing "is lazy"
    (is (lazy? (ccc/some-map identity [1 2 3])))
    (is (lazy? (ccc/some-map identity [1])))
    (let [empty (ccc/some-map identity [])]
      #?(:clj  (is (= (class clojure.lang.PersistentList/EMPTY) (type empty)))
         :cljd (is (= [] empty))
         :cljs (is (lazy? empty))))
    (let [nils (ccc/some-map identity [nil nil nil])]
      #?(:clj  (is (= (class clojure.lang.PersistentList/EMPTY) (type nils)))
         :cljd (is (= [] nils))
         :cljs (is (lazy? nils)))))

  (testing "creates a transducer"
    (is (= [1 2 3] (transduce (ccc/some-map identity) conj [1 2 3])))
    (is (= [2 4] (eduction (ccc/some-map identity) (map inc) [1 nil 3])))
    (is (= [2 4] (sequence (comp (ccc/some-map identity) (map inc)) [1 nil 3])))
    (is (= [] (transduce (ccc/some-map identity) conj [])))
    (is (= [1 3] (transduce (ccc/some-map identity) conj [1 nil 3])))))

(deftest map-set-test
  (is (= #{} (ccc/map-set inc nil)))
  (is (= #{2} (ccc/map-set inc [1])))
  (is (= #{2} (ccc/map-set inc [1 1])))
  (is (= #{2 3 4 5} (ccc/map-set inc [1 1 2 3 4 3]))))

(deftest mapcat-set-test
  (is (= #{} (ccc/mapcat-set vector nil)))
  (is (= #{1} (ccc/mapcat-set vector [1])))
  (is (= #{1} (ccc/mapcat-set vector [1 1])))
  (is (= #{1 2 3 4} (ccc/mapcat-set vector [1 1 2 3 4 3]))))

(deftest map-distinct-test
  (is (= [] (ccc/map-distinct inc nil)))
  (is (= [2] (ccc/map-distinct inc [1])))
  (is (= [2] (ccc/map-distinct inc [1 1])))
  (is (= [2 3 4 5] (ccc/map-distinct inc [1 1 2 3 4 3])))
  (is (= [2 3 4 5] (transduce (ccc/map-distinct inc) conj [1 1 2 3 4 3]))))

(deftest rsort-test
  (testing "a nil collection"
    (is (= [] (ccc/rsort nil))))
  (testing "an empty collection"
    (is (= [] (ccc/rsort []))))
  (testing "a single-element collection"
    (is (= [1] (ccc/rsort [1]))))
  (testing "an already reverse-sorted collection"
    (is (= [5 4 3 2 1] (ccc/rsort [5 4 3 2 1]))))
  (testing "a regular-sorted collection"
    (is (= [5 4 3 2 1] (ccc/rsort [1 2 3 4 5]))))
  (testing "a shuffled collection"
    (is (= [5 4 3 2 1] (ccc/rsort [4 5 1 3 2]))))
  (testing "by custom compare function"
    (is (= [[1 5] [2 4] [3 3] [4 2] [5 1]]
           (ccc/rsort
             (fn [x y] (compare (second x) (second y)))
             [[5 1] [4 2] [3 3] [2 4] [1 5]])))))

(deftest rsort-by-test
  (testing "a nil collection"
    (is (= [] (ccc/rsort-by :x nil))))

  (testing "an empty collection"
    (is (= [] (ccc/rsort-by :x []))))

  (testing "a single-element collection"
    (is (= [{:x 1}] (ccc/rsort-by :x [{:x 1}]))))

  (testing "an already reverse-sorted collection"
    (let [coll [{:x 5} {:x 4} {:x 3} {:x 2} {:x 1}]]
      (is (= (reverse (sort-by :x coll)) (ccc/rsort-by :x coll)))))

  (testing "a regular-sorted collection"
    (let [coll [{:x 1} {:x 2} {:x 3} {:x 4} {:x 5}]]
      (is (= (reverse (sort-by :x coll)) (ccc/rsort-by :x coll)))))

  (testing "a shuffled collection"
    (let [coll [{:x 4} {:x 5} {:x 1} {:x 3} {:x 2}]]
      (is (= (reverse (sort-by :x coll)) (ccc/rsort-by :x coll)))))

  (testing "by custom compare function"
    (let [coll       [{:a [5 1]} {:a [4 2]} {:a [3 3]} {:a [2 4]} {:a [1 5]}]
          compare-fn (fn [x y] (compare (second x) (second y)))]
      (is (= (reverse (sort-by :a compare-fn coll))
             (ccc/rsort-by :a compare-fn coll)))))
  )

(deftest drop-until-test
  (is (= [] (sequence (ccc/drop-until pos?) [])))
  (is (= [] (ccc/drop-until pos? [])))
  (is (= [1 2 3] (sequence (ccc/drop-until pos?) [1 2 3])))
  (is (= [1 2 3] (ccc/drop-until pos? [1 2 3])))
  (is (= [1 2 3 -4] (sequence (ccc/drop-until pos?) [-1 -2 -3 0 1 2 3 -4])))
  (is (= [1 2 3 -4] (ccc/drop-until pos? [-1 -2 -3 0 1 2 3 -4]))))

(deftest take-until-test
  (is (= [] (sequence (ccc/take-until pos?) [])))
  (is (= [] (ccc/take-until pos? [])))
  (is (= [-1 -2 -3] (sequence (ccc/take-until pos?) [-1 -2 -3])))
  (is (= [-1 -2 -3] (ccc/take-until pos? [-1 -2 -3])))
  (is (= [-1 -2 -3 0] (sequence (ccc/take-until pos?) [-1 -2 -3 0 1 2 3 -4])))
  (is (= [-1 -2 -3 0] (ccc/take-until pos? [-1 -2 -3 0 1 2 3 -4]))))

(deftest max-v-test
  (is (nil? (ccc/max-v)))
  (is (nil? (ccc/max-v nil)))
  (is (= 0 (ccc/max-v 0)))
  (is (= 1 (ccc/max-v nil 1)))
  (is (= 1 (ccc/max-v 1 nil)))
  (is (= 1 (ccc/max-v 0 1)))
  (is (= 1 (ccc/max-v 1 0)))
  (is (= "ABC" (ccc/max-v "ABB" "ABC"))))

(deftest min-v-test
  (is (nil? (ccc/min-v)))
  (is (nil? (ccc/min-v nil)))
  (is (= 0 (ccc/min-v 0)))
  (is (nil? (ccc/min-v nil 1)))
  (is (nil? (ccc/min-v 1 nil)))
  (is (= 0 (ccc/min-v 0 1)))
  (is (= 0 (ccc/min-v 1 0)))
  (is (= "ABB" (ccc/min-v "ABB" "ABC"))))

(deftest max-k-test
  (testing "empty collection"
    (is (nil? (ccc/max-by :a []))))
  (testing "one item"
    (is (= {:a 1} (ccc/max-by :a [{:a 1}])))
    (is (= {:a 1} (ccc/max-by :b [{:a 1}]))))
  (testing "two items"
    (is (= {:b 2} (ccc/max-by :b [{:a 1} {:b 2}])))
    (is (= {:b 2} (ccc/max-by :b [{:b 1} {:b 2}])))
    (is (= {:b 3} (ccc/max-by :b [{:b 3} {:b 2}]))))
  (testing "three items"
    (is (= {:b 5} (ccc/max-by :b [{:a 1} {:b 2} {:b 5}])))
    (is (= {:b 3} (ccc/max-by :b [{:b 3} {:b 2} {:b 1}])))
    (is (= {:b 1} (ccc/max-by :b [{:a 3} {:a 2} {:b 1}]))))
  (testing "non-keyword key"
    (is (= {"b" 2} (ccc/max-by "b" [{"b" 1} {"b" 2}])))
    (is (= [5 4 3] (ccc/max-by 0 [[5 4 3] [3 4 5]])))
    (is (= [3 4 5] (ccc/max-by 2 [[5 4 3] [3 4 5]]))))
  (testing "compares instant"
    (let [now      (time/now)
          after-5  (time/after now (time/minutes 5))
          before-5 (time/before now (time/minutes 5))]
      (is (= {:time after-5} (ccc/max-by :time [{:time now} {:time after-5} {:time before-5}])))))
  (testing "with comparer"
    (let [comparer #(cond (and (even? %1) (even? %2)) 0
                          (even? %1) 1
                          :else -1)]
      (is (= {:a 4} (ccc/max-by :a comparer [{:a 4} {:a 3} {:a 5}]))))))

(deftest min-k-test
  (testing "empty collection"
    (is (nil? (ccc/min-by :a []))))

  (testing "one item"
    (is (= {:a 1} (ccc/min-by :a [{:a 1}])))
    (is (= {:a 1} (ccc/min-by :b [{:a 1}]))))

  (testing "two items"
    (is (= {:a 1} (ccc/min-by :b [{:a 1} {:b 2}])))
    (is (= {:b 1} (ccc/min-by :b [{:b 1} {:b 2}])))
    (is (= {:b 2} (ccc/min-by :b [{:b 3} {:b 2}]))))

  (testing "three items"
    (is (= {:a 1} (ccc/min-by :b [{:a 1} {:b 2} {:b 5}])))
    (is (= {:b 1} (ccc/min-by :b [{:b 3} {:b 2} {:b 1}])))
    (is (= {:a 3} (ccc/min-by :b [{:a 3} {:a 2 :b 2} {:b 1}]))))

  (testing "non-keyword key"
    (is (= {"b" 1} (ccc/min-by "b" [{"b" 1} {"b" 2}])))
    (is (= [3 4 5] (ccc/min-by 0 [[5 4 3] [3 4 5]])))
    (is (= [5 4 3] (ccc/min-by 2 [[5 4 3] [3 4 5]]))))

  (testing "compares instant"
    (let [now      (time/now)
          after-5  (time/after now (time/minutes 5))
          before-5 (time/before now (time/minutes 5))]
      (is (= {:time before-5} (ccc/min-by :time [{:time now} {:time after-5} {:time before-5}])))))

  (testing "with comparer"
    (let [comparer #(cond (odd? %1) -1 (odd? %2) 1 :else 0)
          things   [{:a 2} {:a 3} {:a 6}]]
      (is (= {:a 3} (ccc/min-by :a comparer things)))
      (is (= {:a 6} (ccc/min-by :a (#?(:cljd dart-comparator :default comparator) >) things)))
      (is (= {:a 2} (ccc/min-by :a (#?(:cljd dart-comparator :default comparator) <) things))))))

(deftest formats-test
  (is (= "Number 9" (ccc/formats "Number %s" 9))))

(deftest pad-left-test
  (is (nil? (ccc/pad-left nil 0)))
  (is (= " " (ccc/pad-left nil 1)))
  (is (= " " (ccc/pad-left "" 1)))
  (is (= "hello" (ccc/pad-left "hello" 1 "X")))
  (is (= "hello" (ccc/pad-left "hello" 5 "X")))
  (is (= "Xhello" (ccc/pad-left "hello" 6 "X")))
  (is (= "0hello" (ccc/pad-left "hello" 6 0)))
  (is (= "00000hello" (ccc/pad-left "hello" 10 0)))
  (is (= "abc" (ccc/pad-left "abc" 0))))

(deftest pad-right-test
  (is (nil? (ccc/pad-right nil 0)))
  (is (= " " (ccc/pad-right nil 1)))
  (is (= " " (ccc/pad-right "" 1)))
  (is (= "hello" (ccc/pad-right "hello" 0)))
  (is (= "hello " (ccc/pad-right "hello" 6)))
  (is (= "hello" (ccc/pad-right "hello" 1 "X")))
  (is (= "hello" (ccc/pad-right "hello" 5 "X")))
  (is (= "helloX" (ccc/pad-right "hello" 6 "X")))
  (is (= "hello0" (ccc/pad-right "hello" 6 0)))
  (is (= "hello00000" (ccc/pad-right "hello" 10 0))))

(deftest pad-left!-test
  (is (nil? (ccc/pad-left! nil 0)))
  (is (= " " (ccc/pad-left! nil 1)))
  (is (= " " (ccc/pad-left! "" 1)))
  (is (= "" (ccc/pad-left! "hello" 0)))
  (is (= "o" (ccc/pad-left! "hello" 1 "X")))
  (is (= "hello" (ccc/pad-left! "hello" 5 "X")))
  (is (= "Xhello" (ccc/pad-left! "hello" 6 "X")))
  (is (= "0hello" (ccc/pad-left! "hello" 6 0)))
  (is (= "00000hello" (ccc/pad-left! "hello" 10 0))))

(deftest pad-right!-test
  (is (nil? (ccc/pad-right! nil 0)))
  (is (= " " (ccc/pad-right! nil 1)))
  (is (= " " (ccc/pad-right! "" 1)))
  (is (= "" (ccc/pad-right! "hello" 0)))
  (is (= "h" (ccc/pad-right! "hello" 1 "X")))
  (is (= "hello" (ccc/pad-right! "hello" 5 "X")))
  (is (= "hello " (ccc/pad-right! "hello" 6)))
  (is (= "helloX" (ccc/pad-right! "hello" 6 "X")))
  (is (= "hello0" (ccc/pad-right! "hello" 6 0)))
  (is (= "hello00000" (ccc/pad-right! "hello" 10 0))))

(deftest char-code-at-test
  (is (= 65 (ccc/char-code-at "ABC" 0)))
  (is (= 66 (ccc/char-code-at "ABC" 1))))

(deftest first-char-code-test
  (is (= 65 (ccc/first-char-code "ABC")))
  (is (= 67 (ccc/first-char-code "C"))))

(deftest not-blank?-test
  (is (ccc/not-blank? "a"))
  (is (ccc/not-blank? "\r\n\t a "))
  (is (not (ccc/not-blank? "\r")))
  (is (not (ccc/not-blank? "\n")))
  (is (not (ccc/not-blank? "\t")))
  (is (not (ccc/not-blank? " ")))
  (is (not (ccc/not-blank? "\r\n\t "))))

(deftest remove-nils-test
  (is (= {:a 1} (ccc/remove-nils {:a 1 :b nil}))))

(deftest remove-blanks-test
  (is (= {:a 1} (ccc/remove-blanks {:a 1 :b "    "}))))

(deftest ex?-test
  (is (false? (ccc/ex? "Not an exception")))
  (is (true? (ccc/ex? #?(:clj  (Exception. "yup")
                         :cljd (Exception. "yup")
                         :cljs (js/Error. "yup")))))
  #?(:clj (is (false? (ccc/ex? (Throwable.)))))
  #?(:cljd (is (false? (ccc/ex? (Error.))))))

(deftest invoke-test
  (is (zero? (ccc/invoke (fn [] 0))))
  (is (= {:foo :bar} (ccc/invoke identity {:foo :bar})))
  (is (= 6 (ccc/invoke + 1 2 3)))
  (is (= "hello world" (-> {:some-fn str} :some-fn (ccc/invoke "hello" " " "world")))))

(deftest narity-test
  (let [f (ccc/narity (fn [] :foo))]
    (is (= :foo (f)))
    (is (= :foo (f 1 2 3)))))

#?(:cljs
   (deftest gobj-test

     (testing "oget"
       (let [jsobj (clj->js {:foo "foo" :bar "bar"})]
         (is (= "foo" (ccc/oget jsobj :foo)))
         (is (= "bar" (ccc/oget jsobj :bar)))
         (is (= "bang" (ccc/oget jsobj :fizz "bang")))))

     (testing "oset"
       (let [jsobj (clj->js {:foo "foo"})]
         (is (= {"foo" "bar"} (js->clj (ccc/oset jsobj :foo "bar"))))
         (is (= {"foo" "bar" "fizz" "bang"} (js->clj (ccc/oset jsobj :fizz "bang"))))
         (is (= {"foo" "bar" "fizz" "bang"} (js->clj jsobj)))
         (is (= {"foo" "bar"} (js->clj (ccc/oset nil :foo "bar"))))))

     (testing "oget-in"
       (let [jsobj (clj->js {:a1 "a1"
                             :b1 {:a2 "a2"
                                  :b2 {:a3 "a3"
                                       :b3 ["zero" "one" "two"]}}})]
         (is (nil? (ccc/oget-in jsobj [:blah])))
         (is (= :default (ccc/oget-in jsobj [:blah] :default)))
         (is (= "a1" (ccc/oget-in jsobj [:a1])))
         (is (nil? (ccc/oget-in jsobj [:a1 :a2])))
         (is (= "a2" (ccc/oget-in jsobj [:b1 :a2])))
         (is (= "a3" (ccc/oget-in jsobj [:b1 :b2 :a3])))
         (is (= "zero" (ccc/oget-in jsobj [:b1 :b2 :b3 0])))
         (is (= "two" (ccc/oget-in jsobj [:b1 :b2 :b3 2])))))

     (testing "oset-in"

       (testing "assigns values"
         (is (= {"a" "a"} (js->clj (ccc/oset-in nil [:a] "a"))))
         (let [jsobj (js-obj)]
           (is (= jsobj (ccc/oset-in jsobj [] "a")))
           (is (= {"a1" "a1"} (js->clj (ccc/oset-in jsobj [:a1] "a1"))))
           (ccc/oset-in jsobj [:b1 :b2 :b3] "b3")
           (ccc/oset-in jsobj [:c1 2] "c1-2")
           (is (= {"a1" "a1"
                   "b1" {"b2" {"b3" "b3"}}
                   "c1" {"2" "c1-2"}}
                  (js->clj jsobj)))))

       (testing "vectors"
         (let [obj (js-obj "vector" [])]
           #_(should-throw js/TypeError "Cannot create property '0' on vector '[]'"
                           (ccc/oset-in obj ["vector" 0] "I am lost"))
           #_(should-throw js/TypeError "Cannot create property 'a1' on vector '[]'"
                           (ccc/oset-in obj ["vector" "a1"] "Me too"))
           (is (= {"vector" []} (js->clj obj)))))

       (testing "js Arrays"
         (let [obj (js-obj "array" (js/Array.))]
           (ccc/oset-in obj ["array" 0] "I am NOT lost")
           (ccc/oset-in obj ["array" 3] "I also work")
           (is (= {"array" ["I am NOT lost" nil nil "I also work"]} (js->clj obj)))))

       (testing "sets"
         (let [obj (js-obj "set" #{})]
           (is (thrown-with-msg? js/TypeError #"Cannot create property '0' on set '#\{\}'" (ccc/oset-in obj ["set" 0] "SETting zero")))
           (is (thrown-with-msg? js/TypeError #"Cannot create property 'a1' on set '#\{\}'" (ccc/oset-in obj ["set" "a1"] "SETting a1")))
           (is (= {"set" #{}} (js->clj obj)))))

       (testing "lists"
         (let [obj (js-obj "list" (list))]
           (is (thrown-with-msg? js/TypeError #"Cannot create property '0' on list '\(\)'" (ccc/oset-in obj ["list" 0] "I am lost")))
           (is (thrown-with-msg? js/TypeError #"Cannot create property 'a1' on list '\(\)'" (ccc/oset-in obj ["list" "a1"] "Me too")))
           (is (= {"list" (list)} (js->clj obj)))))

       (testing "keywords"
         (let [obj (js-obj "keyword" :k)]
           (is (thrown-with-msg? js/TypeError #"Cannot create property 'k1' on keyword ':k'" (ccc/oset-in obj ["keyword" "k1"] "where did I go?")))
           (is (= {"keyword" :k} (js->clj obj)))))

       (testing "numbers"
         (let [obj (js-obj "number" 1)]
           (is (thrown-with-msg? js/TypeError #"Cannot create property 'one' on number '1'" (ccc/oset-in obj ["number" "one"] "oh no")))
           (is (= {"number" 1} (js->clj obj)))))

       (testing "strings"
         (let [obj (js-obj "string" "s")]
           (is (thrown-with-msg? js/TypeError #"Cannot create property 's1' on string 's'" (ccc/oset-in obj ["string" "s1"] "oh no")))
           (is (= {"string" "s"} (js->clj obj)))))

       (testing `"js Objects"
         (let [obj (js-obj "obj" (js-obj))]
           (ccc/oset-in obj ["obj" "o1"] "hello")
           (is (= {"obj" {"o1" "hello"}} (js->clj obj)))))
       )
     )
   )
