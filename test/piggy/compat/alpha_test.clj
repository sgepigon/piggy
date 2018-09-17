(ns piggy.compat.alpha-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest testing is are]]
            [piggy.compat.alpha :as compat]))

;; Example specs

(s/def ::int int?)
(s/def ::number number?)

(s/def ::arity-0 (s/cat))

(s/def ::int-arity-1 (s/cat :x ::int))
(s/def ::number-arity-1 (s/cat :x ::number))

(s/def ::int-arity-2 (s/cat :x ::int :y ::int))
(s/def ::number-arity-2 (s/cat :x ::number :y ::number))

(s/def ::int-variadic (s/cat :x ::int :y ::int :more (s/* ::int)))
(s/def ::number-variadic (s/cat :x ::number :y ::number :more (s/* ::number)))

(s/def ::fn-int-arity-1 (s/fspec :args ::int-arity-1, :ret ::int))
(s/def ::fn-number-arity-1 (s/fspec :args ::number-arity-1, :ret ::number))

(s/def ::fn-int-arity-2 (s/fspec :args ::int-arity-2, :ret ::int))
(s/def ::fn-number-arity-2 (s/fspec :args ::number-arity-2, :ret ::number))

(s/def ::fn-int-variadic (s/fspec :args ::int-variadic, :ret ::int))
(s/def ::fn-number-variadic (s/fspec :args ::number-variadic, :ret ::number))

(s/fdef plus
  :args ::int-arity-2
  :ret ::int)
(defn- plus [x y] (+ x y))

(s/fdef clojure.core/+
  :args (s/alt :arity-0 ::arity-0
               :number-arity-1 ::number-1-arity
               :number-arity-2 ::number-2-arity
               :number-variadic ::number-variadic)
  :ret ::number)

(defn- fn-args-gen
  "Returns a generator for `::compat/fn-args`"
  []
  (s/gen (s/or :symbol #{`plus `compat/valid}
               :keyword #{::fn-int-arity-2 ::fn-number-arity-2}
               :fspec #{(s/fspec :args (s/* some?) :ret any?)}
               :regex #{(s/* any?)})))

;; testing

(deftest consistency-test
  (testing "Test conforming on the same spec (same fn-args) gives the same answer."
    (let [same? (fn [[_ old new]] (= old new))]
      (is (every? same? (compat/exercise-fn-args `plus `plus 1000)))
      (is (every? same? (compat/exercise-fn-args ::fn-int-arity-2 `plus 1000)))
      (is (every? same? (compat/exercise-fn-args `plus ::fn-int-arity-2 1000))))))
