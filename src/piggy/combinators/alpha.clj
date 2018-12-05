(ns piggy.combinators.alpha
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

  `:gen` generator-fn must be a fn of no args that returns a test.check generator.

  `:frequency` must be a map with integer likelihoods for generating `:old` and
  `:new` (default {:old 1 :new 1}). The likelihood of a given generator being
  chosen is its likelihood divided by the sum of all likelihoods."
  [& {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  `(compat-impl (s/spec ~old) (s/spec ~new) ~gen ~frequency))

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
  "Validate a conformed `compat` map.

  Return `::s/invalid` for breaking changes, otherwise return the conformed
  `compat` map."
  [conformed]
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
        ;; Use `s/invalid?` for speed and to avoid making an implicit call to
        ;; `s/conform*` in `s/valid?`.
        (cond
          (s/invalid? x) x
          (s/invalid? (:old x)) (s/unform* new (:new x))
          :else (s/unform* old (:old x))))
      (explain* [_ path via in x]
        (let [old-prob (s/explain* old (conj path :old) via in x)
              new-prob (s/explain* new (conj path :new) via in x)]
          ((fnil into []) old-prob new-prob)))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (sgen/frequency [[(:old frequency) (s/gen* old overrides path rmap)]
                           [(:new frequency) (s/gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn frequency))
      (describe* [_] `(compat :old ~(when old (s/describe* old))
                              :new ~(when new (s/describe* new)))))))

;; `fcompat` implementation

(defn- compatize
  "Return a `compat` spec from a keyword `k` (:args, :ret, or :fn) in fspecs
  `old`and `new`. If neither `old` or `new` have a spec for `k`, return nil."
  [old new frequency k]
  (case [(some? (k old)) (some? (k new))]
    [true true] (compat :old (k old) :new (k new) :frequency frequency)
    [true false] (compat :old (k old) :frequency frequency)
    [false true] (compat :new (k new) :frequency frequency)
    [false false] nil))

(defn- call-valid?
  "Returns true if `f` and `args` are valid for `specs`, otherwise returns falsey.

  See `s/call-valid?`."
  [f specs args]
  (let [cargs (s/conform (:args specs) args)]
    (when-not (s/invalid? cargs)
      (let [ret (apply f args)
            cret (s/conform (:ret specs) ret)]
        (and (not (s/invalid? cret))
             (if (:fn specs)
               (s/valid? (:fn specs) {:args cargs :ret cret})
               true))))))

(defn- validate-fn
  "Returns `f` if valid for `specs`, else the smallest shrunk.

  See `s/validate-fn`."
  [f specs iters]
  ;; Use `s/gen*` because `s/gen` suppresses invalid generated values. To test
  ;; the args spec, we need to pass potentially invalid values.
  (let [g (s/gen* (:args specs) nil [] {::s/recursion-limit s/*recursion-limit*})
        prop (sgen/for-all* [g] #(call-valid? f specs %))]
    (let [ret (sgen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

(defn- spec-name
  "If `spec` has a name, return it, otherwise nil.

  See `s/spec-name`."
  [spec]
  (cond
    (ident? spec) spec
    (s/regex? spec) (::s/name spec)
    (instance? clojure.lang.IObj spec) (::s/name (meta spec))))

(defn- deep-resolve
  "Resolve `pred` to a spec/regex.

  See `s/deep-resolve`."
  [pred]
  (if (ident? pred)
    (recur (s/get-spec pred))
    pred))

(defn- explain
  "Return a collection of `s/explain` problems, which is a map with at least keys
  `:path`, `:pred`, `:val`, `:via`, and `:in`.

  See `s/explain-1`."
  ([spec path via in v]
   (explain nil spec path via in v))
  ([form pred path via in v]
   ;; try and resolve `pred` to a spec
   (let [pred (or (deep-resolve pred) (s/spec? pred) (s/regex? pred))]
     (if (s/spec? pred)
       (s/explain* pred path (if-let [name (spec-name pred)] (conj via name) via) in v)
       [{:path path :pred form :val v :via via :in in}]))))

(defn fcompat-impl
  "Do not call this directly, use `fcompat`."
  [old new gfn frequency]
  (let [compatize-kw (partial compatize old new frequency)
        specs {:old old
               :new new
               :args (compatize-kw :args)
               :ret (compatize-kw :ret)
               :fn (compatize-kw :fn)}]
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
      ;; TODO explain*
      (explain* [_ path via in f]
        (if (ifn? f)
          (let [args (validate-fn f specs *fcompat-iterations*)]
            ;; return nil on success. Like `s/fspec`, might not be reproducible.
            (when-not (identical? f args)
              (let [cargs (s/conform (:args specs) args)]
                (if (s/invalid? cargs)
                  (explain (:args specs) (conj path :args) via in args)
                  (let [ret (try (apply f args) (catch Throwable t t))]
                    (if (instance? Throwable ret)
                      ;; When `s/fspec` adds exception data, add exception data.
                      [{:path path :pred '(apply fn) :val args :via via :in in :reason (.getMessage ^Throwable ret)}]
                      (let [cret (s/conform (:ret specs) ret)]
                        (if (s/invalid? cret)
                          (explain (:ret specs) (conj path :ret) via in ret)
                          (when (:fn specs)
                            (explain (:fn specs) (conj path :fn) via in {:args cargs :ret cret}))))))))))
          (explain 'ifn? ifn? path via in f)))
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
