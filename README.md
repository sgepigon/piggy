# piggy

> “Ralph made a step forward and Jack smacked Piggy’s head. Piggy’s
> glasses flew off and tinkled on the rocks. Piggy cried out in terror:
> ‘My specs\! One side’s broken.”

— William Golding, *Lord of the Flies*, Chapter 4

`piggy` is a Clojure library that helps with broken
[specs](https://clojure.org/about/spec).

## Installation

Download from <https://github.com/sgepigon/piggy>.

## Usage

Let's say we have a spec'ed function and we want to see if a new spec is
compatible:

``` clojure
(ns piggy.compat.readme
  (:require [clojure.spec.alpha :as s]
            [piggy.compat.alpha :as compat]))

(s/def ::int int?)
(s/def ::number number?)
(s/def ::old (s/fspec :args (s/cat :x ::int :y ::int), :ret ::int))
(s/def ::new (s/fspec :args (s/cat :a ::number :b ::number), :ret ::number))

(s/fdef plus
  :args (s/cat :x ::int :y ::int)
  :ret ::int)
(defn- plus [x y] (+ x y))
```

We can use `compat/exercise` to see how the our old and new specs
compare.

``` clojure
(compat/exercise `plus ::new)
(compat/exercise ::old ::new)
(compat/exercise (s/cat :x ::int :y ::int) ::new)
```

The three calls to `compat/exercise` above are equivalent and all return
a sequence of `[val old-conformed-val new-conformed-val]` tuples, e.g.:

``` clojure
([(-1 0) {:x -1, :y 0} {:a -1, :b 0}]
 [(0 0) {:x 0, :y 0} {:a 0, :b 0}]
 [(-1 0) {:x -1, :y 0} {:a -1, :b 0}]
 [(-1 -2) {:x -1, :y -2} {:a -1, :b -2}]
 [(-4 1) {:x -4, :y 1} {:a -4, :b 1}]
 [(-1 1) {:x -1, :y 1} {:a -1, :b 1}]
 [(-13 -1) {:x -13, :y -1} {:a -13, :b -1}]
 [(6 58) {:x 6, :y 58} {:a 6, :b 58}]
 [(1 3) {:x 1, :y 3} {:a 1, :b 3}]
 [(0 0) {:x 0, :y 0} {:a 0, :b 0}])
```

We see that the new spec is compatible because `::number` subsumes
`::int`.

To see what happens when specs are incompatible, let's switch the
arguments:

``` clojure
(compat/exercise ::new ::old)
```

``` clojure
([(-1 -1) {:a -1, :b -1} {:x -1, :y -1}]
 [(-1.0 -2.0) {:a -1.0, :b -2.0} :clojure.spec.alpha/invalid]
 [(0 0.5) {:a 0, :b 0.5} :clojure.spec.alpha/invalid]
 [(2 -4) {:a 2, :b -4} {:x 2, :y -4}]
 [(0.625 -3) {:a 0.625, :b -3} :clojure.spec.alpha/invalid]
 [(15 0.0) {:a 15, :b 0.0} :clojure.spec.alpha/invalid]
 [(1.5 0.5) {:a 1.5, :b 0.5} :clojure.spec.alpha/invalid]
 [(0.765625 -0.5) {:a 0.765625, :b -0.5} :clojure.spec.alpha/invalid]
 [(12 -3) {:a 12, :b -3} {:x 12, :y -3}]
 [(1.9375 0) {:a 1.9375, :b 0} :clojure.spec.alpha/invalid])
```

`::old` conform on the int values of `::new`, but is invalid for doubles
that `::new` generates.

## Developers

Run Clojure unit tests with:

``` example
lein test
```

## License

Copyright © 2018 Santiago Gepigon III

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
