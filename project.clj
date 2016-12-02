(defproject suzuki-kasami "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aleph "0.4.2-alpha10"]
                 [manifold "0.1.5"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [com.taoensso/timbre "4.7.4"]
                 [environ "1.0.0"]]
  :main ^:skip-aot suzuki-kasami.resource-server
  :target-path "target/%s"
  :uberjar-name "resource-server.jar"
  :profiles {:uberjar {:aot :all}})
