(ns piggy.compat.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]))
;; specs

(s/def ::fargs (s/or :symbol qualified-symbol?
                     :keyword qualified-keyword?
                     :fspec s/spec?
                     :regex s/regex?))

;; conformer specs

;; use conformers to resolve specs to the function arguments (fargs) and
;; return ::s/invalid if not found
(s/def ::fspec->fargs (s/and (s/conformer #(:args %)) some?))
(s/def ::sym-or-kw->fargs (s/and (s/conformer s/get-spec) ::fspec->fargs))
(s/def ::->fargs (s/or :symbol (s/and qualified-symbol? ::sym-or-kw->fargs)
                       :keyword (s/and qualified-keyword? ::sym-or-kw->fargs)
                       :fspec (s/and s/spec? ::fspec->fargs)
                       :regex s/regex?))

(s/fdef valid
  :args (s/cat :x any?)
  :ret (complement s/invalid?))
(defn- valid
  "Return `x` if `x` is a valid s/conform return value, nil otherwise."
  [x]
  (when ((complement s/invalid?) x) x))

(s/fdef exercise-args
  :args (s/alt :default (s/cat :old ::fargs, :new ::fargs)
               :n-samples (s/cat :old ::fargs, :new ::fargs, :n nat-int?)
               :overrides (s/cat :old ::fargs, :new ::fargs,
                                 :n nat-int?, :overrides (s/nilable map?)))
  :ret (s/every vector?))
(defn exercise-args
  "Generates a number (default 10) of values compatible with `old` and conforms
  the `old` and `new` specs over them, returning a sequence of
  [val old-conformed-val new-conformed-val] tuples. For values that don't
  conform to `new`, new-conformed-val will be ::s/invalid. Optionally takes a
  generator overrides map as per gen for `old`."
  ([old new] (exercise-args old new 10))
  ([old new n] (exercise-args old new n nil))
  ([old new n overrides]
   (let [->fargs #(when-let [[_ args] (valid (s/conform ::->fargs %))] args)
         -old (->fargs old)
         -new (->fargs new)]
     (if (and -old -new)
       (map #(vector % (s/conform -old %) (s/conform -new %))
            (sgen/sample (s/gen -old overrides) n))
       (throw (ex-info "No :args spec found, can't generate"
                       (as-> {} m
                         (if-not -old (assoc m :old (s/form old)) m)
                         (if-not -new (assoc m :new (s/form new)) m))))))))
