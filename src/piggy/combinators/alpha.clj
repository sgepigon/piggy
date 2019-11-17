(ns piggy.combinators.alpha
  (:refer-clojure :exclude [reverse])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(def ^:dynamic *fcompat-iterations*
  "The number of times an anonymous fn specified by `fcompat` will
  be (generatively) tested during `s/explain`.

  The default number of times (100) is the the same as `s/fspec` for
  `s/explain`. Use `s/*fspec-iterations*` to control testing during
  `s/conform`."
  100)

;;;; Combinator Macros

(defmacro compat
  "Takes `:old` and `:new` kwargs whose values are predicates or specs and returns
  a spec that returns a map of the `:old` and `:new` conformed values.

  Optionally takes `:gen` generator-fn or `:frequency` likelihood map.

  `:gen` generator-fn must be a fn of no args that returns a test.check
  generator.

  `:frequency` must be a map with integer likelihoods for generating `:old` and
  `:new` (default {:old 1 :new 1}). The likelihood of a given generator being
  chosen is its likelihood divided by the sum of all likelihoods."
  [& {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  `(compat-impl (s/spec ~old) (s/spec ~new) ~gen ~frequency))

(defmacro reverse
  "Takes a `compat` spec and returns a spec with the compatibility between `:old`
  and `:new` reversed.

  Optionally takes `:gen` generator-fn or `:frequency` likelihood map. See
  `compat` a full description of `:gen` and `:frequency`."
  [spec & {:keys [gen frequency]}]
  `(reverse-impl (s/spec ~spec) ~gen ~frequency))

(defmacro fcompat
  "Takes `:old` and `:new` kwargs whose values are fspecs and returns a spec whose
  `s/conform` or `s/explain` take a fn and validates compatibility between
  `:old` and `:new` using generative testing. The conformed value is always the
  fn itself.

  fcompats can generate functions that validate the arguments for compatibility
  and fabricate a return value compliant with the `compat` :ret spec, ignoring
  the `compat` :fn spec if present.

  Optionally takes `:gen` generator-fn or `:frequency` likelihood map. See
  `compat` a full description of `:gen` and `:frequency`."
  [& {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  `(fcompat-impl (s/spec ~old) (s/spec ~new) ~gen ~frequency))

;;;; Implementations

;; `compat` implementation

(defn- validate
  "Return `::s/invalid` for breaking changes for the key `k`, otherwise return
  `conformed`."
  ([conformed]
   (validate :new conformed))
  ([k conformed]
   (if (s/invalid? (k conformed)) (k conformed) conformed)))

(defn compat-impl
  "Do not call this directly, use `compat`."
  [old new gfn frequency]
  (let [specs {::op `compat :old old :new new :gen gfn :frequency frequency}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [_ x] (validate {:old (s/conform* old x) :new (s/conform* new x)}))
      (unform* [_ x] (if (s/invalid? x) x (s/unform* new (:new x))))
      (explain* [_ path via in x]
        (when-let [new-prob (s/explain* new (conj path :new) via in x)]
          (let [old-prob (s/explain* old (conj path :old) via in x)]
            (into (vec old-prob) new-prob))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (sgen/frequency [[(:old frequency) (s/gen* old overrides path rmap)]
                           [(:new frequency) (s/gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn frequency))
      (describe* [_] `(compat :old ~(when old (s/describe* old))
                              :new ~(when new (s/describe* new)))))))

;; `reverse` implementation

(defn- reverse?
  "Return true if `spec` is a reverse spec."
  [spec]
  (boolean (= `reverse (::op spec))))

(defn reverse-impl
  "Do not call this directly, use `reverse`."
  [{:keys [old new] :as spec} gfn frequency]
  (let [frequency (or frequency (:frequency spec))
        gfn (or gfn (:gen spec))
        specs {::op `reverse
               :spec spec
               :old old
               :new new
               :gen gfn
               :frequency frequency}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [_ x]
        (if (reverse? spec)
          (s/conform* (:spec spec) x)
          (validate :old {:old (s/conform* old x) :new (s/conform* new x)})))
      (unform* [_ x]
        (if (reverse? spec)
          (s/unform* (:spec spec) x)
          (if (s/invalid? x) x (s/unform* old (:old x)))))
      (explain* [_ path via in x]
        (if (reverse? spec)
          (s/explain* (:spec spec) path via in x)
          (when-let [old-prob (s/explain* old (conj path :old) via in x)]
            (let [new-prob (s/explain* new (conj path :new) via in x)]
              (into (vec old-prob) new-prob)))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (if (reverse? spec)
            (s/gen* (:spec spec) overrides path rmap)
            (sgen/frequency [[(:old frequency) (s/gen* new overrides path rmap)]
                             [(:new frequency) (s/gen* old overrides path rmap)]]))))
      (with-gen* [_ gfn] (reverse-impl spec gfn frequency))
      (describe* [_] `(reverse ~(s/describe* spec))))))

;; `fcompat` implementation

(defn- compatize
  "Return a `compat` spec from a keyword `k` (:args, :ret, or :fn) in fspecs
  `old`and `new`. If neither `old` or `new` have a spec for `k`, return nil."
  [old new frequency k]
  (case (map (comp some? k) [old new])
    [true true] (compat :old (k old) :new (k new) :frequency frequency)
    [true false] (compat :old (k old) :frequency frequency)
    [false true] (compat :new (k new) :frequency frequency)
    [false false] nil))

(defn- validate-fn
  "Returns `f` if valid for `specs`, else the smallest shrunk.

  See `s/validate-fn`."
  [f specs iters]
  ;; Use `s/gen*` because `s/gen` suppresses invalid generated values. To test
  ;; the args spec, we need to pass potentially invalid values.
  (let [g (s/gen* (:args specs) nil [] {::s/recursion-limit s/*recursion-limit*})
        prop (sgen/for-all* [g] #(#'s/call-valid? f specs %))
        {:keys [seed] :as ret} (sgen/quick-check iters prop)]
    (if-let [[smallest] (-> ret :shrunk :smallest)]
      {:args smallest :seed seed}
      ;; Check that the return is more constrained
      (let [rgen (s/gen* (:ret specs) nil [] {::s/recursion-limit s/*recursion-limit*})
            rprop (sgen/for-all* [rgen] #(s/valid? (:ret specs) %))
            rret (sgen/quick-check iters rprop :seed seed)]
        (if-let [[rsmallest] (-> rret :shrunk :smallest)]
          {:problem :less-constrained :ret rsmallest :seed seed}
          f)))))

(defn fcompat-impl
  "Do not call this directly, use `fcompat`."
  [old new gfn frequency]
  (let [compatize-kw (partial compatize old new frequency)
        specs {::op `fcompat
               :old old
               :new new
               :args (compatize-kw :args)
               :ret (reverse (compatize-kw :ret))
               :fn (compatize-kw :fn)
               :gen gfn
               :frequency frequency}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [this f]
        (if (:args specs)
          (if (ifn? f)
            (if (identical? f (validate-fn f specs s/*fspec-iterations*)) f ::s/invalid)
            ::s/invalid)
          (throw (Exception. (str "Can't conform fcompat without args spec in both :old and :new fspecs: \n" (pr-str (s/describe this)))))))
      (unform* [_ f] f)
      (explain* [_ path via in f]
        (if (ifn? f)
          (let [validated (validate-fn f specs *fcompat-iterations*)]
            ;; return nil on success. Like `s/fspec`, might not be reproducible.
            (when-not (identical? f validated)
              (let [{vargs :args vret :ret problem :problem} validated]
                (if (= problem :less-constrained)
                  (let [explain (#'s/explain-1 nil (:ret specs) (conj path :ret) via in vret)
                        pred (get-in explain [0 :pred])
                        reason (format "%s: %s is less constrained than %s"
                                       pred
                                       (s/describe (:new (:ret specs)))
                                       (s/describe (:old (:ret specs))))]
                    (assoc-in explain [0 :reason] reason))
                  (let [cargs (s/conform (:args specs) vargs)]
                    (if (s/invalid? cargs)
                      (#'s/explain-1 nil (:args specs) (conj path :args) via in vargs)
                      (let [ret (try (apply f vargs) (catch Throwable t t))]
                        (if (instance? Throwable ret)
                          ;; When `s/fspec` adds exception data, add exception data.
                          [{:path path :pred '(apply fn) :val vargs :via via :in in :reason (.getMessage ^Throwable ret)}]
                          (let [cret (s/conform (:ret specs) ret)]
                            (if (s/invalid? cret)
                              (#'s/explain-1 nil (:ret specs) (conj path :ret) via in ret)
                              (when (:fn specs)
                                (#'s/explain-1 nil (:fn specs) (conj path :fn) via in {:args cargs :ret cret}))))))))))))
          (#'s/explain-1 'ifn? ifn? path via in f)))
      (gen* [_ overrides _ _]
        (if gfn
          (gfn)
          (sgen/return
           (fn [& args]
             (assert (s/valid? (:args specs) args) (with-out-str (s/explain (:args specs) args)))
             (sgen/generate (s/gen (:ret specs) overrides))))))
      (with-gen* [_ gfn] (fcompat-impl old new gfn frequency))
      (describe* [_] `(fcompat :old ~(when old (s/describe* old))
                               :new ~(when new (s/describe* new)))))))
