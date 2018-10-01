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
  :args (s/* ::number)
  :ret ::number)

(defn- fn-args-gen
  "Returns a generator for `::compat/fn-args`"
  []
  (s/gen (s/or :symbol #{`plus `compat/valid}
               :keyword #{::fn-int-arity-2 ::fn-number-arity-2}
               :fspec #{(s/fspec :args (s/* some?) :ret any?)}
               :regex #{(s/* any?)})))

;; testing

(defn- both-valid?
  "Checks that both the `old` and `new` conformed values are valid."
  [[_ old new]]
  (and (not= ::s/invalid old) (not= ::s/invalid new)))

(defn- breaking?
  "Checks that the `new` conformed value is invalid."
  [[_ _ new]]
  (s/invalid? new))

(deftest consistency-test
  (testing "Test conforming on the same spec (same fn-args) gives the same answer."
    (let [same? (fn [[_ old new]] (= old new))]
      (is (every? same? (compat/exercise-fn-args `plus `plus 1000)))
      (is (every? same? (compat/exercise-fn-args ::fn-int-arity-2 `plus 1000)))
      (is (every? same? (compat/exercise-fn-args `plus ::fn-int-arity-2 1000))))))

(deftest relaxation-test
  (testing "For function arguments, test that requiring less does not break the
  spec."
    (is (every? both-valid? (compat/exercise-fn-args ::fn-int-arity-2 ::fn-number-arity-2 1000))
        "`::number-arity-2` is more general than `::int-arity-2`.")
    (is (every? both-valid? (compat/exercise-fn-args ::fn-number-arity-2 ::fn-number-variadic 1000))
        "`::number-variadic` is more general than `::number-arity-2`.")
    (is (every? both-valid? (compat/exercise-fn-args ::fn-int-arity-2 ::fn-number-variadic 1000))
        "`::number-variadic` is more general than `::int-arity-2`.")))

(deftest accretion-test
  (testing "For function return values, Test providing more does not break the spec."))

(deftest breakage-test
  (testing "For function arguments, test requiring more breaks the spec."
    (is (some breaking? (compat/exercise-fn-args ::fn-number-arity-2 ::fn-int-arity-2 1000))
        "`::int-arity-2` is narrower than `::number-arity-2`.")
    (is (some breaking? (compat/exercise-fn-args ::fn-number-variadic ::fn-number-arity-2 1000))
        "`::number-arity-2` is narrower than `::number-variadic`.")
    (is (some breaking? (compat/exercise-fn-args ::fn-number-variadic ::fn-int-arity-2 1000))
        "`::int-arity-2` is narrower than `::number-variadic`."))
  (testing "For function return values, Test providing less breaks the spec."))
