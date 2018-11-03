(ns piggy.combinators.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defn compat-impl
  "TODO Do not call this directly, use `compat`."
  [old new]
  (reify
    s/Specize
    (specize* [s] s)
    (specize* [s _] s)
    s/Spec
    (conform* [_ x] {:old (s/conform old x) :new (s/conform new x)})
    ;; TODO how should it unform? Like s/cat? Cant's specize nil
    ;; Doesn't work with `::s/invalid`
    (unform* [_ x]
      (or (s/unform* old (first x))
          (s/unform* new (second x))))
    (explain* [_ path via in x]
      ;; TODO Still need to figure out explain semantics
      (apply merge (mapv #(s/explain* % path via in x) [old new])))
    ;; TODO how should overrides work?
    (gen* [_ overrides path rmap]
      (let [gen* (fn [x] (s/gen* x overrides path rmap))]
        (sgen/frequency [[1 (gen* old)] [1 (gen* new)]])))
    ;; TODO how should overrides work?
    (with-gen* [_ gfn] (mapv #(s/with-gen* % gfn) [old new]))
    (describe* [_] `(compat :old ~(s/form old) :new ~(s/form new)))))

(defmacro compat
  "TODO"
  [& {:keys [old new]}]
  `(compat-impl (s/spec ~old) (s/spec ~new)))

