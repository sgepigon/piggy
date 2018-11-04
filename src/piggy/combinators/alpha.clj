(ns piggy.combinators.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defn compat-impl
  "TODO Do not call this directly, use `compat`."
  [old new gfn]
  (let [specs {:old old :new new}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

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
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (sgen/frequency [[1 (s/gen* old overrides path rmap)]
                           [1 (s/gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn))
      (describe* [_] `(compat :old ~(s/form old) :new ~(s/form new))))))

(s/fdef compat
  :args (s/keys* :req-un [::old ::new] :opt-un [::gen]))
(defmacro compat
  "TODO"
  [& {:keys [old new gen]}]
  `(compat-impl (s/spec ~old) (s/spec ~new) ~gen))

