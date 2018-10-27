(ns piggy.combinators.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defn- ->pair
  "Make a pair map."
  [f old new]
  {:old (f old), :new (f new)})

(defn pair-impl
  "TODO A spec wrapper."
  [left right]
  (reify
    s/Specize
    (specize* [s] s)
    (specize* [s _] s)
    s/Spec
    (conform* [_ x] (->pair #(s/conform* % x) left right))
    ;; TODO how should it unform? Like s/cat? Cant's specize nil
    ;; Doesn't work with `::s/invalid`
    (unform* [_ x]
      (or (s/unform* left (first x))
          (s/unform* right (second x))))
    (explain* [_ path via in x]
      ;; TODO Still need to figure out explain semantics
      (apply merge (mapv #(s/explain* % path via in x) [left right])))
    ;; TODO how should overrides work?
    (gen* [_ overrides path rmap]
      (let [gen* (fn [x] (s/gen* x overrides path rmap))]
        (sgen/frequency [[1 (gen* left)] [1 (gen* right)]])))
    ;; TODO how should overrides work?
    (with-gen* [_ gfn] (mapv #(s/with-gen* % gfn) [left right]))
    ;; (describe* [_] `(pair ~(s/form left) ~(s/form right)))
    (describe* [_] (list `pair (s/form left) (s/form right)))))

(defmacro pair
  "TODO"
  [left right]
  `(piggy.compat.alpha/pair-impl (s/spec ~left) (s/spec ~right)))
