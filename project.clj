(defproject aoc2022 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  ;:license {:name "Eclipse Public License"
  ;          :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :main ^:skip-aot aoc2022.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
