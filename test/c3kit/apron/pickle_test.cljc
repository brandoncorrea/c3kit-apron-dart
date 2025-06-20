(ns c3kit.apron.pickle-test
  (:require [c3kit.apron.pickle :as sut]
            [c3kit.apron.time :as time]
            [clojure.test :refer [deftest is testing]]))

(deftype Wallace [cheese invention]
  sut/Pickleable
  (pickleable-type [_] :pickle-spec/wallace)
  (pickleable->map [_] {:cheese cheese :invention invention}))

(defmethod sut/map->pickleable :pickle-spec/wallace [_ {:keys [cheese invention]}]
  (Wallace. cheese invention))

(defrecord Gromit [expression paper]
  sut/Pickleable
  (pickleable-type [_] :pickle-spec/gromit)
  (pickleable->map [this] this))

(defmethod sut/map->pickleable :pickle-spec/gromit [_ m] (map->Gromit m))

(deftest pickle-test

  (testing "scalars pickle"
    (is (nil? (sut/pickle nil)))
    (is (zero? (sut/pickle 0)))
    (is (= 42 (sut/pickle 42)))
    (is (= 0.0 (sut/pickle 0.0)))
    (is (= 3.14 (sut/pickle 3.14)))
    (is (= "foo" (sut/pickle "foo")))
    (is (= :bar (sut/pickle :bar)))
    (is (= 'fizz (sut/pickle 'fizz))))

  (testing "scalars unpickle"
    (is (nil? (sut/unpickle nil)))
    (is (zero? (sut/unpickle 0)))
    (is (= 42 (sut/unpickle 42)))
    (is (= 0.0 (sut/unpickle 0.0)))
    (is (= 3.14 (sut/unpickle 3.14)))
    (is (= "foo" (sut/unpickle "foo")))
    (is (= :bar (sut/unpickle :bar)))
    (is (= 'fizz (sut/unpickle 'fizz))))

  #_(it "long strings"
        (should= 16 sut/LONG_STRING_LENGTH)
        (let [s       "This ia a long string that might be duplicated in the graph so it gets pickled as a ref."
              id      (sut/-unique-id s)
              pickled (sut/pickle s)]
          (should= {:_refs {id s} :_object {:_t :ref :_v id}} pickled)
          (should= s (sut/unpickle pickled))))

  (testing "instant"
    (let [s       (time/utc 2025 02 28 7 25 0)
          id      (sut/-unique-id s)
          pickled (sut/pickle s)]
      (is (= {:_refs {id {:_t :inst :_v (pr-str s)}} :_object {:_t :ref :_v id}} pickled))
      (is (= s (sut/unpickle pickled)))))

  (testing "list"
    (let [s       [1 2 3]
          id      (sut/-unique-id s)
          pickled (sut/pickle s)]
      (is (= {:_refs {id {:_t :seq :_v [1 2 3]}} :_object {:_t :ref :_v id}} pickled))
      (is (= s (sut/unpickle pickled)))))

  (testing "set"
    (let [s       #{1 2 3}
          id      (sut/-unique-id s)
          pickled (sut/pickle s)]
      (is (= :set (get-in pickled [:_refs id :_t])))
      (is (= s (set (get-in pickled [:_refs id :_v]))))
      ;(should= {:_refs {id {:_t :set :_v [1 2 3]}} :_object {:_t :ref :_v id}} pickled)
      (is (= s (sut/unpickle pickled)))))

  (testing "map"
    (let [m       {:foo "bar"}
          id      (sut/-unique-id m)
          pickled (sut/pickle m)]
      (is (= {:_refs {id {:_t :map, :_v {:foo "bar"}}} :_object {:_t :ref :_v id}} pickled))
      (is (= m (sut/unpickle pickled)))))

  (testing "list with a map"
    (let [foobar  {:foo "bar"}
          obj     [foobar]
          pickled (sut/pickle obj)]
      (is (= obj (sut/unpickle pickled)))
      (is (contains? (:_refs pickled) (sut/-unique-id foobar)))
      (is (contains? (:_refs pickled) (sut/-unique-id obj)))))

  (testing "map with a list"
    (let [foobar  [:foo "bar"]
          obj     {:list foobar}
          pickled (sut/pickle obj)]
      (is (= obj (sut/unpickle pickled)))
      (is (contains? (:_refs pickled) (sut/-unique-id foobar)))
      (is (contains? (:_refs pickled) (sut/-unique-id obj)))))

  (testing "duplicates maps in a list"
    (let [foobar  {:foo "bar"}
          obj     [foobar foobar]
          pickled (sut/pickle obj)]
      (is (= obj (sut/unpickle pickled)))
      (is (contains? (:_refs pickled) (sut/-unique-id foobar)))
      (is (contains? (:_refs pickled) (sut/-unique-id obj)))))

  (testing "defrecord"
    (let [gromit  (->Gromit "annoyed" "times")
          id      (sut/-unique-id gromit)
          pickled (sut/pickle gromit)]
      (is (= {:_refs {id {:_t :pickle-spec/gromit, :_v (into {} gromit)}} :_object {:_t :ref :_v id}} pickled))
      (is (= gromit (sut/unpickle pickled)))))

  (testing "deftype"
    (let [wallace   (->Wallace "munster" "rocket")
          id        (sut/-unique-id wallace)
          pickled   (sut/pickle wallace)
          unpickled (sut/unpickle pickled)]
      (is (= {:_refs   {id {:_t :pickle-spec/wallace :_v {:cheese "munster" :invention "rocket"}}}
              :_object {:_t :ref :_v id}} pickled))
      (is (instance? Wallace unpickled))
      (is (= (sut/pickleable->map wallace) (sut/pickleable->map unpickled)))))

  ;; MDM - cycles are not currently supported.
  ;;  They should only be possible in CLJS or using non-Clojure types.
  ;#?(:cljs
  ;   (focus-it "cyclic dependency - deftype"
  ;     (let [wallace   (->Wallace "munster" "rocket")
  ;           _         (set! (.-invention wallace) wallace)
  ;           id        (sut/unique-id wallace)
  ;           pickled   (sut/pickle wallace)
  ;           unpickled (sut/unpickle pickled)]
  ;       (should= {:_refs   {id {:_refs   {1 {:_t :pickle-spec/wallace
  ;                                              :_v {:cheese "munster" :invention {:_t :ref :_v id}}}}
  ;                                 :_object {:_t :ref :_v id}}}
  ;                   :_object {:_t :ref :_v id}} pickled)
  ;       (should= Wallace (type unpickled))
  ;       (should= (sut/pickleable->map wallace) (sut/pickleable->map unpickled)))))

  )