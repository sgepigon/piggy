(ns piggy.compat.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))
;; specs

(s/def ::fn-args (s/or :symbol qualified-symbol?
                     :keyword qualified-keyword?
                     :fspec s/spec?
                     :regex s/regex?))

;; conformer specs

;; use conformers to resolve specs to the function arguments (fn-args) and
;; return ::s/invalid if not found
(s/def ::fspec->fn-args (s/and (s/conformer #(:args %)) some?))
(s/def ::sym-or-kw->fn-args (s/and (s/conformer s/get-spec) ::fspec->fn-args))
(s/def ::->fn-args (s/or :symbol (s/and qualified-symbol? ::sym-or-kw->fn-args)
                       :keyword (s/and qualified-keyword? ::sym-or-kw->fn-args)
                       :fspec (s/and s/spec? ::fspec->fn-args)
                       :regex s/regex?))

(s/fdef valid
  :args (s/cat :x any?)
  :ret (complement s/invalid?))
(defn- valid
  "Return `x` if `x` is a valid s/conform return value, nil otherwise."
  [x]
  (when ((complement s/invalid?) x) x))

(s/fdef exercise-fn-args
  :args (s/alt :default (s/cat :old ::fn-args, :new ::fn-args)
               :n-samples (s/cat :old ::fn-args, :new ::fn-args, :n nat-int?)
               :overrides (s/cat :old ::fn-args, :new ::fn-args,
                                 :n nat-int?, :overrides (s/nilable map?)))
  :ret (s/every vector?))
(defn exercise-fn-args
  "Generates a number (default 10) of values compatible with `old`'s args spec and
  conforms the `old` and `new` specs over them, returning a sequence of
  [val old-conformed-val new-conformed-val] tuples. For values that don't
  conform to `new`, new-conformed-val will be ::s/invalid. Optionally takes a
  generator overrides map as per gen for `old`."
  ([old new] (exercise-fn-args old new 10))
  ([old new n] (exercise-fn-args old new n nil))
  ([old new n overrides]
   (let [->fn-args #(when-let [[_ args] (valid (s/conform ::->fn-args %))] args)
         old-args (->fn-args old)
         new-args (->fn-args new)]
     (if (and old-args new-args)
       (map #(vector % (s/conform old-args %) (s/conform new-args %))
            (sgen/sample (s/gen old-args overrides) n))
       (throw (ex-info "No :args spec found, can't generate"
                       (as-> {} m
                         (if-not old-args (assoc m :old (s/form old)) m)
                         (if-not new-args (assoc m :new (s/form new)) m))))))))
