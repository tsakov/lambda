(defproject lambda "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.incubator "0.1.3"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot lambda.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
