(ns piggy.combinators.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/fdef compat
  :args (s/keys* :req-un [::old ::new] :opt-un [::gen ::frequency]))
(defmacro compat
  "Takes `:old` and `:new` kwargs whose values are predicates or specs and returns
  a spec that returns a map of the `:old` and `:new` conformed values.

  Optionally takes `:gen` generator-fn or `:frequency` likelihood map.

  `:gen` generator-fn must be a fn of no args that returns a test.check generator.

  `:frequency` must be a map with integer likelihoods for generating `:old` and
  `:new` (default {:old 1 :new 1}). The likelihood of a given generator being
  chosen is its likelihood divided by the sum of all likelihoods."
  [& {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  `(compat-impl (s/spec ~old) (s/spec ~new) ~gen ~frequency))

(s/fdef fcompat
  :args (s/keys* :req-un [::old ::new] :opt-un [::gen]))
(defmacro fcompat
  "TODO"
  [& {:keys [old new gen]}]
  `(fcompat-impl (s/spec ~old) (s/spec ~new) ~gen))

(defn- validate
  "Validate a conformed `compat` map.

  Return `::s/invalid` for breaking changes, otherwise return the conformed
  `compat` map."
  [conformed]
  (s/assert (s/keys :req-un [::old ::new]) conformed)
  (case (map (complement s/invalid?) ((juxt :old :new) conformed))
    [true true] conformed
    [true false] ::s/invalid
    [false true] conformed
    [false false] ::s/invalid))

(defn compat-impl
  "Do not call this directly, use `compat`."
  [old new gfn frequency]
  (let [specs {:old old :new new}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [_ x] (validate {:old (s/conform* old x) :new (s/conform* new x)}))
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
          (sgen/frequency [[(:old frequency) (s/gen* old overrides path rmap)]
                           [(:new frequency) (s/gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn frequency))
      (describe* [_] `(compat :old ~(when old (s/describe* old))
                              :new ~(when new (s/describe* new)))))))

(defn fcompat-impl
  "Do not call this directly, use `fcompat`."
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
      (conform* [_ f] {:old (s/conform old f) :new (s/conform new f)})
      (unform* [_ f]
        (if (s/invalid? (:old f))
          (s/unform* new (:new f))
          (s/unform* old (:old f))))
      (explain* [_ path via in f]
        (let [old-prob (s/explain* old (conj path :old) via in f)
              new-prob (s/explain* new (conj path :new) via in f)]
          (into old-prob new-prob)))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (sgen/frequency [[1 (s/gen* old overrides path rmap)]
                           [1 (s/gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn))
      (describe* [_] `(fcompat :old ~(s/form old) :new ~(s/form new))))))


