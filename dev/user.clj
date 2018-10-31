(ns user
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]))

;; http://clojure-goes-fast.com/blog/performance-nemesis-reflection/
(set! *warn-on-reflection* true)

(alter-var-root #'s/*explain-out* (constantly (expound/custom-printer
                                               {:show-valid-values? true
                                                :print-specs? false
                                                :theme :figwheel-theme})))
