(ns piggy.compat.alpha
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]))


(comment
  "Example specs"

  (s/def ::int int?)
  (s/def ::number number?)

  (s/fdef plus
    :args (s/cat :x ::int :y ::int)
    :ret ::int)

  (s/def ::old (s/fspec :args (s/cat :x ::int :y ::int), :ret ::int))

  (s/def ::new (s/fspec :args (s/cat :x ::int :y ::int), :ret ::int))

  (defn plus [x y] (+ x y))
  )
