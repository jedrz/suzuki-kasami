(defproject suzuki-kasami "0.1.0-SNAPSHOT"
  :description "Suzuki-Kasami algorithm"
  :url "https://github.com/jedrz/suzuki-kasami"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aleph "0.4.2-alpha10"]
                 [manifold "0.1.5"]
                 [gloss "0.2.6"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [com.taoensso/timbre "4.7.4"]
                 [environ "1.0.0"]
                 [ring-logger-timbre "0.7.5"]
                 [ring.middleware.conditional "0.2.0"]]
  :min-lein-version "2.6.1"
  :aot [suzuki-kasami.resource-server suzuki-kasami.node]
  :target-path "target/%s"
  :profiles {:resource-server {:main suzuki-kasami.resource-server
                               :uberjar-name "resource-server.jar"}
             :node {:main suzuki-kasami.node
                    :uberjar-name "node.jar"}})
