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

Let's say we're writing a spec for `clojure.core/+`. We might start out
like this:

``` clojure
(ns piggy.readme
  (:require [clojure.spec.alpha :as s]
            [piggy.combinators.alpha :as pc]))

(+ 1 2) ; => 3
(s/def ::int-fn (s/fspec :args (s/cat :x int? :y int?) :ret int?))
(s/conform (s/get-spec ::int-fn) +) ; => #function[clojure.core/+]
```

We soon realize that we forgot that `clojure.core/+` also works over
other numbers.

``` clojure
(+ 1.5 2.5) ; => 4.0
(+ 1/2 3/4) ; => 5/4
```

We update our spec
accordingly:

``` clojure
(s/def ::number-fn (s/fspec :args (s/cat :x number? :y number?) :ret number?))
(s/conform (s/get-spec ::number-fn) +) ; => #function[clojure.core/+]
```

How do we know change isn't a *breaking change*? `fcompat` is a spec
combinator that encodes a compatibility property between two specs.
`fcompat` is a spec itself, so the same functions that work on
`clojure.spec` specs (`s/conform`, `s/unform`, `s/explain`, `s/gen`,
`s/with-gen`, and `s/describe`) work on `compat` and
`fcompat`.

``` clojure
(s/conform (pc/fcompat :old ::int-fn :new ::number-fn) +) ; => #function[clojure.core/+]

;; If we switch `:old` and `:new` we get a breaking change (i.e. we require more and provide less)
(s/conform (pc/fcompat :new ::int-fn :old ::number-fn) +) ; => :clojure.spec.alpha/invalid
(s/explain (pc/fcompat :new ::int-fn :old ::number-fn) +)
;; => 1.0 - failed: int? in: [0] at: [:args :new :x]
```

But wait\! We also forgot `clojure.core/+` is variadic:

``` clojure
(+) ; => 0
(+ 1.0) ; => 1.0
(+ 0 1 2 3 4 5 6 7 8 9) ; => 45
```

Let's update our spec again and see if it's
compatible:

``` clojure
(s/def ::variadic-number-fn (s/fspec :args (s/* number?) :ret number?)) ; variadic
(s/conform (s/get-spec ::variadic-number-fn) +) ; => #function[clojure.core/+]


(s/conform (pc/fcompat :old ::number-fn :new ::variadic-number-fn) +) ; => #function[clojure.core/+]

;; Again, switching `:old` and `:new` gives us a breaking change
(s/conform (pc/fcompat :new ::number-fn :old ::variadic-number-fn) +) ; => :clojure.spec.alpha/invalid
(s/explain (pc/fcompat :new ::number-fn :old ::variadic-number-fn) +)
;; => () - failed: Insufficient input at: [:args :new :x]
```

## Developers

Run Clojure unit tests with:

``` example
lein test
```

## License

Copyright © 2018 Santiago Gepigon III

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
