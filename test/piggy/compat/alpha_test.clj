(ns piggy.compat.alpha-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [piggy.compat.alpha :as compat]))


;; Example specs

(s/def ::int int?)
(s/def ::number number?)
(s/def ::old (s/fspec :args (s/cat :x ::int :y ::int), :ret ::int))
(s/def ::new (s/fspec :args (s/cat :x ::number :y ::number), :ret ::number))

(s/fdef plus
  :args (s/cat :x ::int :y ::int)
  :ret ::int)
(defn- plus [x y] (+ x y))

(defn- ->args-gen
  "Returns a generator for `::compat/->args`"
  []
  (s/gen (s/or :symbol #{`plus `compat/valid}
               :keyword #{::old ::new}
               :fspec #{(s/fspec :args (s/* some?) :ret any?)}
               :regex #{(s/* any?)})))

;; testing

(deftest consistency-test
  (testing "Test conforming on the same spec gives the same answer."
    (let [same? (fn [[_ old new]] (= old new))]
      (is (apply = true (map same? (compat/exercise-args `plus `plus 1000))))
      (is (apply = true (map same? (compat/exercise-args ::old `plus 1000))))
      (is (apply = true (map same? (compat/exercise-args `plus ::old 1000)))))))
