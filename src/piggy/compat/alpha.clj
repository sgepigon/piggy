(ns piggy.compat.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]))

;; conformer specs
(s/def ::fspec->args (s/and (s/conformer #(:args %)) some?))
(s/def ::sym-or-kw->args (s/and (s/conformer s/get-spec) ::fspec->args))
(s/def ::->args (s/or :symbol (s/and qualified-symbol? ::sym-or-kw->args)
                      :keyword (s/and qualified-keyword? ::sym-or-kw->args)
                      :fspec (s/and s/spec? ::fspec->args)
                      :regex s/regex?))

(s/fdef valid
  :args (s/cat :x any?)
  :ret (complement s/invalid?))
(defn- valid
  "Return `x` if `x` is a valid s/conform return value, nil otherwise."
  [x]
  (when ((complement s/invalid?) x) x))

(s/fdef exercise-args
  :args (s/alt :default (s/cat :old ::->args, :new ::->args)
               :n-samples (s/cat :old ::->args, :new ::->args, :n nat-int?)
               :overrides (s/cat :old ::->args, :new ::->args,
                                 :n nat-int?, :overrides (s/nilable map?)))
  :ret (s/every (s/cat :val any?, :old-conformed-val any?, :new-conformed-val any?)))
(defn exercise-args
  "Generates a number (default 10) of values compatible with `old` and conforms
  the `old` and `new` specs over them, returning a sequence of
  [val old-conformed-val new-conformed-val] tuples. For values that don't
  conform to `new`, new-conformed-val will be ::s/invalid. Optionally takes a
  generator overrides map as per gen for `old`."
  ([old new] (exercise-args old new 10))
  ([old new n] (exercise-args old new n nil))
  ([old new n overrides]
   (let [->args-spec #(when-let [[_ args] (valid (s/conform ::->args %))] args)
         -old (->args-spec old)
         -new (->args-spec new)]
     (if (and -old -new)
       (map #(vector % (s/conform -old %) (s/conform -new %))
            (sgen/sample (s/gen -old overrides) n))
       (throw (ex-info "No :args spec found, can't generate"
                       (as-> {} m
                         (if-not -old (assoc m :old (s/form old)) m)
                         (if-not -new (assoc m :new (s/form new)) m))))))))
