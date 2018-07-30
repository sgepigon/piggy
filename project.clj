(defproject piggy "0.1.0-SNAPSHOT"
  :description "Test for spec compatibility and breaking changes."
  :url "https://github.com/sgepigon/piggy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[expound "0.7.1"]
                                  [org.clojure/test.check "0.9.0"]]}})
