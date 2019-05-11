(defproject piggy "0.1.0-SNAPSHOT"
  :description "Test for spec compatibility and breaking changes."
  :url "https://github.com/sgepigon/piggy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[expound "0.7.2"]
                                  [org.clojure/test.check "0.10.0"]]}})
