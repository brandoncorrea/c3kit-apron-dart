(ns c3kit.apron.bad-words-test
  (:require [c3kit.apron.bad-words :as sut]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            ))

(defn expand-patterns [pattern-set]
  (reduce (fn [acc pattern]
            (let [base-word    (str/replace pattern "*" "")
                  example-word (str/replace pattern "*" "x")]
              (conj acc base-word example-word)))
          #{}
          pattern-set))

(def variation-words (expand-patterns sut/patterns))

(def test-words
  #{"@ss"
    "4ss"
    "4ssh0l3"
    "c0ck"
    "sh!t"
    "sh!+"
    "b!+ch"
    "b17ches"
    "f4gg!t"
    "motha fuckah"})

(def bad-phrases
  #{"asshol3"
    "shitty"
    "sh1tty"
    "hell?"
    "shi+"
    "shi+y"
    "bullshit"
    "bullsh1t"
    "bullsh!t"
    "bullsh1tty"
    "what the hell?"})

(def safe-phrases
  #{""
    "hello"
    "dam"
    "shirt"
    "Hell0 there!"
    "shitaki"
    "shellfish"
    "assemble"})

(deftest not-profanity
  (doseq [phrase safe-phrases]
    (is (not (sut/contains-profanity? phrase)))))

(deftest profanity
  (doseq [word (concat bad-phrases
                       sut/words
                       test-words
                       variation-words)]
    (is (sut/contains-profanity? word))))

