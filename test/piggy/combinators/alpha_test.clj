(ns piggy.combinators.alpha-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [piggy.combinators.alpha :as pc]))

(def ^:private valid?
  "Return true if not `::s/invalid`"
  (complement s/invalid?))

(deftest reverse-test
  (let [int-to-num (pc/compat :old int? :new number?)
        num-to-int (pc/compat :old number? :new int?)]
    (testing "Plain reverse"
      (is (= (s/conform num-to-int 1.0)
             (s/conform (pc/reverse int-to-num) 1.0)))
      (is (= (s/conform num-to-int 1)
             (s/conform (pc/reverse int-to-num) 1)))
      (is (= int-to-num (:spec (pc/reverse int-to-num))))
      (is (= num-to-int (:spec (pc/reverse num-to-int)))))
    (testing "Reverse, reverse"
      (is (= (s/conform int-to-num 1)
             (s/conform (pc/reverse (pc/reverse int-to-num)) 1)))
      (is (= (s/conform num-to-int 1.0)
             (s/conform (pc/reverse (pc/reverse num-to-int)) 1.0))))
    (testing "Reverse, reverse, reverse"
      (is (= (-> num-to-int (s/conform 1))
             (-> int-to-num pc/reverse pc/reverse pc/reverse (s/conform 1))))
      (is (and (valid? (-> int-to-num (s/conform 1.0)))
               (valid? (-> num-to-int pc/reverse pc/reverse pc/reverse (s/conform 1.0))))))))
