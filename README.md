# piggy

> “Ralph made a step forward and Jack smacked Piggy’s head. Piggy’s
> glasses flew off and tinkled on the rocks. Piggy cried out in terror:
> ‘My specs\! One side’s broken.”

— William Golding, *Lord of the Flies*, Chapter 4

`piggy` is a Clojure library that helps with broken
[specs](https://clojure.org/about/spec).

## Project Status

Will be in alpha as long as `clojure.spec` is in alpha.

## Installation

Download from <https://github.com/sgepigon/piggy>.

## Usage

Let's say we're writing a spec for `clojure.core/+`. We might start out
like this:

``` clojure
(ns piggy.demo.readme
  (:require
   [clojure.spec.alpha :as s]
   [piggy.combinators.alpha :as pc]))

(+ 2 2)
;; => 4

(s/fspec :args (s/cat :x int? :y int?)
         :ret int?)

;; `s/fspec` returns the function itself on a successful conform
(s/conform (s/fspec :args (s/cat :x int? :y int?)
                    :ret int?)
           +)
;; => #function[clojure.core/+]

;; Can also use `s/explain` to print a human readable message:
(s/explain (s/fspec :args (s/cat :x int? :y int?)
                    :ret int?)
           +)
;; => Success!
```

We think about it a little more and realize `+` is variadic:

``` clojure
(+) ; => 0
(+ 1) ; => 1
(+ 0 1 2 3 4 5 6 7 8 9) ; => 45
```

We can use `s/*` for zero or more ints.

``` clojure
(s/explain (s/fspec :args (s/* int?)
                    :ret int?)
           +)
;; => (-8782045379102980082 -441326657751795727) - failed: integer overflow
```

Two things are going on here:

1.  Because we switch from a 2-arity—`(s/cat :x int? y: int?)`—to
    variadic—`(s/* int?)`—we're hitting integer overflow.

2.  Turns out `clojure.core/+` doesn't auto-promote. But there is a
    built-in Clojure function, `clojure.core/+'`, that does arbitrary
    precision.
    
    So let's try this with `+'`.

<!-- end list -->

``` clojure
(s/explain (s/fspec :args (s/* int?)
                    :ret int?)
           +')
;; => 9223372036854775808N - failed: int? at: [:ret]
```

We can see it auto-promotes but now our return spec is wrong. We also
realize `+'` takes all sorts of numbers.

``` clojure
(+ -1 3.14 22/7 0x77)
;; => 124.28285714285714
```

Let's update `int?` to `number?`

``` clojure
(s/explain (s/fspec :args (s/* number?)
                    :ret number?)
           +')
;; => Success!
```

Cool, we're more comfortable with this spec. How do we know change isn't
a *breaking change*?

`compat` and `fcompat` are spec combinators that encodes a compatibility
property between two specs.

`compat` is for simple comparsions over specs and predicates while
`fcompat` is used to compare compatibility between functions. `s/spec`
is to `compat` as `s/fspec` is to `fcompat`.

The combinators are is a spec themselves, so the same functions that
work on `clojure.spec` specs (`s/conform`, `s/unform`, `s/explain`,
`s/gen`, `s/with-gen`, and `s/describe`) work on `compat` and `fcompat`.

``` clojure
(s/explain (pc/fcompat :old (s/fspec :args (s/cat :x int? :y int?)
                                     :ret int?)
                       :new (s/fspec :args (s/* number?)
                                     :ret number?))
           +')
;; => 1.0 - failed: int? at: [:ret :old]
```

We see here it's a breaking change: we promised an `int?` but now we're
saying we're returning a `number?`—this is *weakening* a promise.

We can show it trivially with `clojure.core/any?`

``` clojure
(s/explain (pc/fcompat :old (s/fspec :args (s/* number?)
                                     :ret number?)
                       :new (s/fspec :args (s/* number?)
                                     :ret any?))
           +')
;; => \space - failed: clojure.core/number?: any? is less constrained than
;; number? at: [:ret :old]
```

Similarlity, we can show a breaking change by requiring more in the
arguments:

``` clojure
(s/explain (pc/fcompat :old (s/fspec :args (s/* number?)
                                     :ret number?)
                       :new (s/fspec :args (s/cat :x number? :y number?)
                                     :ret number?))
           +')
;; => () - failed: Insufficient input at: [:args :new :x]
```

## Developers

Run Clojure unit tests with either:

``` example
lein test
```

or

``` example
clj -A:test
```

## License

Copyright © 2018–2019 Santiago Gepigon III

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
