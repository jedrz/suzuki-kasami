(ns suzuki-kasami.node
   (:require
    [manifold.deferred :as d]
    [manifold.stream :as s]
    [aleph.tcp :as tcp]
    [taoensso.timbre :as log])
   (:gen-class))

(defn handle-message
  [msg]
  (log/info "Got message" (String. msg)))

(defn handler
  [s info]
  (log/info "New connection" info)
  (d/chain
   (s/take! s)
   (fn [msg]
     (s/close! s)
     (d/future (handle-message msg)))))

(defn start-server
  [options]
  (tcp/start-server handler options))

(defn -main
  [& args]
  (println "Node"))
