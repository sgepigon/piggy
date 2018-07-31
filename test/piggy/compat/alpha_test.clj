(ns piggy.compat.alpha-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [piggy.compat.alpha :as compat]))

(s/fdef with-examples
  :args (s/cat :spec s/spec? :examples set?)
  :ret any?)
(defn- with-examples
  "Takes a spec and a set of examples and returns a version of that spec that uses
  that generator of `examples`."
  [spec examples]
  (s/with-gen spec #(s/gen examples)))

;; Example specs

(s/def ::int int?)
(s/def ::number number?)
(s/def ::old (s/fspec :args (s/cat :x ::int :y ::int), :ret ::int))
(s/def ::new (s/fspec :args (s/cat :x ::number :y ::number), :ret ::number))

(s/fdef plus
  :args (s/cat :x ::int :y ::int)
  :ret ::int)
(defn- plus [x y] (+ x y))

(s/def ::->args-gen
  (s/or :symbol (with-examples (s/and qualified-symbol? ::compat/sym-or-kw->args) #{`plus `compat/valid})
        :keyword (with-examples (s/and qualified-keyword? ::compat/sym-or-kw->args) #{::old ::new})
        :fspec (with-examples (s/and s/spec? ::compat/fspec->args) #{(s/fspec :args (s/* some?) :ret any?)})
        :regex (with-examples s/regex? #{(s/* any?)})))

;; testing

(deftest consistency-test
  (testing "Test conforming on the same spec gives the same answer."
    (let [same? (fn [[_ old new]] (= old new))]
      (is (apply = true (map same? (compat/exercise-args `plus `plus 1000))))
      (is (apply = true (map same? (compat/exercise-args ::old `plus 1000))))
      (is (apply = true (map same? (compat/exercise-args `plus ::old 1000)))))))
