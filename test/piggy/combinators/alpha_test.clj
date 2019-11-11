(ns piggy.combinators.alpha-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [piggy.combinators.alpha :as pc]))

;; Example specs

(s/def ::int int?)
(s/def ::number number?)

(s/def ::arity-0 (s/cat))

(s/def ::int-arity-1 (s/cat :x ::int))
(s/def ::number-arity-1 (s/cat :x ::number))

(s/def ::int-arity-2 (s/cat :x ::int :y ::int))
(s/def ::number-arity-2 (s/cat :x ::number :y ::number))

(s/def ::int-variadic (s/* ::int))
(s/def ::number-variadic (s/* ::number))

(s/def ::fn-int-arity-1 (s/fspec :args ::int-arity-1, :ret ::int))
(s/def ::fn-number-arity-1 (s/fspec :args ::number-arity-1, :ret ::number))

(s/def ::fn-int-arity-2 (s/fspec :args ::int-arity-2, :ret ::int))
(s/def ::fn-number-arity-2 (s/fspec :args ::number-arity-2, :ret ::number))

(s/def ::fn-int-variadic (s/fspec :args ::int-variadic, :ret ::int))
(s/def ::fn-number-variadic (s/fspec :args ::number-variadic, :ret ::number))

(s/fdef clojure.core/+
  :args (s/* ::number)
  :ret ::number)

;; testing

(def ^:private valid?
  "Return true if not `::s/invalid`"
  (complement s/invalid?))

(deftest reverse-test
  (let [int-to-num (pc/compat :old ::int :new ::number)
        num-to-int (pc/compat :old ::number :new ::int)]
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

(deftest consistency-test
  (binding [pc/*fcompat-iterations* 1000]
    (testing "test conforming on the same spec gives the same answer."
      (is (-> (pc/fcompat :old (s/get-spec `+)
                          :new (s/get-spec `+))
              (s/conform +)
              valid?))
      (is (-> (pc/fcompat :old ::fn-number-variadic
                          :new (s/get-spec `+))
              (s/conform +)
              valid?))
      (is (-> (pc/fcompat :old (s/get-spec `+)
                          :new ::fn-number-variadic)
              (s/conform +)
              valid?)))))

(defn- spec-ulation-fn
  "Example from Spec-ulation"
  [version]
  (fn [{:keys [num wheat donkey] :as input}]
    (cond-> {}
      ;; "You say: I am going to provide you more. You were giving me seven
      ;; before, and I gave you back 42, and now I am going to give you back 42
      ;; and some wheat. More stuff."
      (= num 7)                          (merge {:num 42})
      (and (= version :new) (= num 7))   (merge {:wheat 2})
      ;; "The other thing is relaxation. It used to be you give me two wheat and a
      ;; donkey, and I will give you some steel. And now, I do not need the
      ;; donkey. Just give me wheat and I will give you steel. So I require less.
      ;; That is a relaxation on my part."
      (and (= wheat 2) (= donkey 1))     (merge {:steel 1})
      (and (= version :new) (= wheat 2)) (merge {:steel 1}))))

(s/def ::num (s/with-gen ::int #(s/gen #{7})))
(s/def ::wheat (s/with-gen ::int #(s/gen #{2})))
(s/def ::donkey (s/with-gen ::int #(s/gen #{1})))
(s/def ::steel ::int)
(s/def ::provide-num
  (s/fspec :args (s/cat :input (s/keys :req-un [::num]))
           :ret (s/keys :req-un [::num])))
(s/def ::provide-num-and-wheat
  (s/fspec :args (s/cat :input (s/keys :req-un [::num]))
           :ret (s/keys :req-un [::num ::wheat])))

(deftest accretion-test
  (binding [pc/*fcompat-iterations* 1000]
    (testing "For function return values, test providing more does not break the
  spec."
      (testing "Spec-ulation example."
        (is (-> (pc/fcompat :old ::provide-num
                            :new ::provide-num-and-wheat)
                (s/conform (spec-ulation-fn :old))
                valid?))
        (is (-> (pc/fcompat :old ::provide-num
                            :new ::provide-num-and-wheat)
                (s/conform (spec-ulation-fn :new))
                valid?))))))

(s/def ::relax-wheat-and-donkey
  (s/fspec :args (s/cat :input (s/keys :req-un [::wheat ::donkey]))
           :ret (s/keys :req-un [::steel])))
(s/def ::relax-wheat
  (s/fspec :args (s/cat :input (s/keys :req-un [::wheat]))
           :ret (s/keys :req-un [::steel])))

(deftest relaxation-test
  (binding [pc/*fcompat-iterations* 1000]
    (testing "For function arguments, test that requiring less does not break the
  spec."
      (is (-> (pc/fcompat :old (s/fspec :args ::int-arity-2)
                          :new (s/fspec :args ::number-arity-2))
              (s/conform +)
              valid?))
      (is (-> (pc/fcompat :old (s/fspec :args ::number-arity-2)
                          :new (s/fspec :args ::number-variadic))
              (s/conform +)
              valid?))
      (is (-> (pc/fcompat :old (s/fspec :args ::int-arity-2)
                          :new (s/fspec :args ::number-variadic))
              (s/conform +)
              valid?))
      (testing "Spec-ulation example."
        (is (-> (pc/fcompat :old ::relax-wheat-and-donkey
                            :new ::relax-wheat)
                (s/conform (spec-ulation-fn :new))
                valid?))))))

(deftest breakage-test
  (binding [pc/*fcompat-iterations* 1000]
    (testing "For function arguments, test requiring more breaks the spec."

      (is (-> (pc/fcompat :old (s/fspec :args ::number-arity-2)
                          :new (s/fspec :args ::int-arity-2))
              (s/conform +)
              s/invalid?))
      (is (-> (pc/fcompat :old (s/fspec :args ::number-variadic)
                          :new (s/fspec :args ::number-arity-2))
              (s/conform +)
              s/invalid?))
      (is (-> (pc/fcompat :old (s/fspec :args ::number-variadic)
                          :new (s/fspec :args ::int-arity-2))
              (s/conform +)
              s/invalid?))
      (testing "Spec-ulation example."
        (is (-> (pc/fcompat :old ::relax-wheat
                            :new ::relax-wheat-and-donkey)
                (s/conform (spec-ulation-fn :old))
                s/invalid?))))

    (testing "Strengthening a promise (the return) is a compatibile change."
      (is (-> (pc/fcompat :old (s/fspec :args ::number-variadic :ret (s/nilable ::number))
                          :new (s/fspec :args ::number-variadic :ret ::number))
              (s/conform +)
              valid?)))

    (testing "TODO Weakening a promise (the return) is a breaking change."
      (comment
        (is (-> (pc/fcompat :old (s/fspec :args ::number-variadic :ret ::number)
                            :new (s/fspec :args ::number-variadic :ret any?))
                (s/conform +)
                s/invalid?))))

    (testing "For function return values, test providing less breaks the spec."
      (testing "Spec-ulation example."
        (is (-> (pc/fcompat :old ::provide-num-and-wheat
                            :new ::provide-num)
                (s/conform (spec-ulation-fn :old))
                s/invalid?))))))
