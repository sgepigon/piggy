(ns piggy.combinators.alpha2
  (:refer-clojure :exclude [reverse])
  (:require
   [clojure.spec-alpha2 :as s]
   [clojure.spec-alpha2.gen :as sgen]
   [clojure.spec-alpha2.impl :as simpl]
   [clojure.spec-alpha2.protocols :as protocols
    :refer [conform* unform* explain* gen* with-gen* describe*]]))

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
  [& opts]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(compat ~@opts))))

(defmacro reverse
  "Takes a `compat` spec and returns a spec with the compatibility between `:old`
  and `:new` reversed.

  Optionally takes `:gen` generator-fn or `:frequency` likelihood map. See
  `compat` a full description of `:gen` and `:frequency`."
  [spec & opts]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(reverse ~spec ~@opts))))

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
  [& opts]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(fcompat ~@opts))))

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

      protocols/Spec
      (conform* [_ x settings-key settings]
        (validate {:old (conform* old x settings-key settings)
                   :new (conform* new x settings-key settings)}))
      (unform* [_ x] (if (s/invalid? x) x (unform* new (:new x))))
      (explain* [_ path via in x settings-key settings]
        (when-let [new-prob (explain* new (conj path :new) via in x settings-key settings)]
          (let [old-prob (explain* old (conj path :old) via in x settings-key settings)]
            (into (vec old-prob) new-prob))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (sgen/frequency [[(:old frequency) (gen* old overrides path rmap)]
                           [(:new frequency) (gen* new overrides path rmap)]])))
      (with-gen* [_ gfn] (compat-impl old new gfn frequency))
      (describe* [_] `(compat :old ~(when old (describe* old))
                              :new ~(when new (describe* new)))))))

(defmethod s/expand-spec `compat
  [[_ & {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]]
  {:clojure.spec/op `compat
   :old old
   :new new
   :gen gen
   :frequency frequency})

(defmethod s/create-spec `compat
  [{:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  (compat-impl (s/resolve-spec old)
               (s/resolve-spec new)
               (s/resolve-fn gen)
               frequency))

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

      protocols/Spec
      (conform* [_ x settings-key settings]
        (if (reverse? spec)
          (conform* (:spec spec) x settings-key settings)
          (validate :old {:old (conform* old x settings-key settings)
                          :new (conform* new x settings-key settings)})))
      (unform* [_ x]
        (if (reverse? spec)
          (unform* (:spec spec) x)
          (if (s/invalid? x) x (unform* old (:old x)))))
      (explain* [_ path via in x settings-key settings]
        (if (reverse? spec)
          (explain* (:spec spec) path via in x settings-key settings)
          (when-let [old-prob (explain* old (conj path :old) via in x settings-key settings)]
            (let [new-prob (explain* new (conj path :new) via in x settings-key settings)]
              (into (vec old-prob) new-prob)))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (if (reverse? spec)
            (gen* (:spec spec) overrides path rmap)
            (sgen/frequency [[(:old frequency) (gen* new overrides path rmap)]
                             [(:new frequency) (gen* old overrides path rmap)]]))))
      (with-gen* [_ gfn] (reverse-impl spec gfn frequency))
      (describe* [_] `(reverse ~(describe* spec))))))

(defmethod s/expand-spec `reverse
  [[_ spec & {:keys [gen frequency]}]]
  {:clojure.spec/op `reverse
   :spec spec
   :gen gen
   :frequency frequency})

(defmethod s/create-spec `reverse
  [{:keys [spec gen frequency]}]
  (reverse-impl (s/resolve-spec spec)
                (s/resolve-fn gen)
                frequency))

;; `fcompat` implementation

(defn- compatize
  "Return a `compat` symbolic spec map from a keyword `k` (:args, :ret, or :fn) in
  fspecs `old`and `new`. If neither `old` or `new` have a spec for `k`, return
  nil."
  [old new frequency k]
  (when (and (k old) (k new))
    {:clojure.spec/op `compat
     :old (when (k old) (describe* (k old)))
     :new (when (k new) (describe* (k new)))
     :frequency frequency}))

(defn- validate-fn
  "Returns `f` if valid for `specs`, else the smallest shrunk.

  See `simpl/validate-fn`."
  [f specs iters]
  ;; Use `gen*` because `s/gen` suppresses invalid generated values. To test
  ;; the args spec, we need to pass potentially invalid values.
  (let [g (gen* (:args specs) nil [] {::s/recursion-limit s/*recursion-limit*})
        prop (sgen/for-all* [g] #(#'simpl/call-valid? f specs %))
        {:keys [seed] :as ret} (sgen/quick-check iters prop)]
    (if-let [[smallest] (-> ret :shrunk :smallest)]
      {:args smallest :seed seed}
      ;; Check that the return is more constrained
      (let [rgen (gen* (:ret specs) nil [] {::s/recursion-limit s/*recursion-limit*})
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
               :args (s/create-spec (compatize-kw :args))
               :ret (s/create-spec {:clojure.spec/op `reverse
                                    :spec (compatize-kw :ret)})
               :fn (s/create-spec (compatize-kw :fn))
               :gen gfn
               :frequency frequency}]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (get specs k))
      (valAt [_ k not-found] (get specs k not-found))

      protocols/Spec
      (conform* [this f settings-key settings]
        (if (:args specs)
          (if (ifn? f)
            (if (identical? f (validate-fn f specs s/*fspec-iterations*)) f ::s/invalid)
            ::s/invalid)
          (throw (Exception. (str "Can't conform fcompat without args spec in both :old and :new fspecs: \n" (pr-str (s/describe this)))))))
      (unform* [_ f] f)
      (explain* [_ path via in f settings-key settings]
        (if (ifn? f)
          (let [validated (validate-fn f specs *fcompat-iterations*)]
            ;; return nil on success. Like `s/fspec`, might not be reproducible.
            (when-not (identical? f validated)
              (let [{vargs :args vret :ret problem :problem} validated]
                (if (= problem :less-constrained)
                  (let [explain (#'simpl/explain-1 nil (:ret specs) (conj path :ret) via in vret settings-key settings)
                        pred (get-in explain [0 :pred])
                        reason (format "%s: %s is less constrained than %s"
                                       pred
                                       (describe* (:new (:ret specs)))
                                       (describe* (:old (:ret specs))))]
                    (assoc-in explain [0 :reason] reason))
                  (let [cargs (s/conform (:args specs) vargs)]
                    (if (s/invalid? cargs)
                      (#'simpl/explain-1 nil (:args specs) (conj path :args) via in vargs settings-key settings)
                      (let [ret (try (apply f vargs) (catch Throwable t t))]
                        (if (instance? Throwable ret)
                          ;; When `s/fspec` adds exception data, add exception data.
                          [{:path path :pred '(apply fn) :val vargs :via via :in in :reason (.getMessage ^Throwable ret)}]
                          (let [cret (s/conform (:ret specs) ret)]
                            (if (s/invalid? cret)
                              (#'simpl/explain-1 nil (:ret specs) (conj path :ret) via in ret settings-key settings)
                              (when (:fn specs)
                                (#'simpl/explain-1 nil (:fn specs) (conj path :fn) via in {:args cargs :ret cret} settings-key settings))))))))))))
          (#'simpl/explain-1 'ifn? ifn? path via in f settings-key settings)))
      (gen* [_ overrides _ _]
        (if gfn
          (gfn)
          (sgen/return
           (fn [& args]
             (assert (s/valid? (:args specs) args) (with-out-str (s/explain (:args specs) args)))
             (sgen/generate (s/gen (:ret specs) overrides))))))
      (with-gen* [_ gfn] (fcompat-impl old new gfn frequency))
      (describe* [_] `(fcompat :old ~(when old (describe* old))
                               :new ~(when new (describe* new)))))))

(defmethod s/expand-spec `fcompat
  [[_ & {:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]]
  {:clojure.spec/op `fcompat
   :old old
   :new new
   :gen gen
   :frequency frequency})

(defmethod s/create-spec `fcompat
  [{:keys [old new gen frequency] :or {frequency {:old 1 :new 1}}}]
  (fcompat-impl (s/resolve-spec old)
                (s/resolve-spec new)
                (s/resolve-fn gen)
                frequency))
