(ns piggy.combinators.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defn compat-impl
  "Do not call this directly, use `compat`."
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
      (unform* [_ x]
        (if (s/invalid? (:old x))
          (s/unform* new (:new x))
          (s/unform* old (:old x))))
      (explain* [_ path via in x]
        (let [old-prob (s/explain* old (conj path :old) via in x)
              new-prob (s/explain* new (conj path :new) via in x)]
          (into old-prob new-prob)))
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

