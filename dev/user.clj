(ns user
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]
            [piggy.compat.alpha :as compat]))

(alter-var-root #'s/*explain-out* (constantly (expound/custom-printer
                                               {:show-valid-values? true
                                                :print-specs? false
                                                :theme :figwheel-theme})))
