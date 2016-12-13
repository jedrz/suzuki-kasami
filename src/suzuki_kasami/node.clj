(ns suzuki-kasami.node
   (:require
    [manifold.deferred :as d]
    [manifold.stream :as s]
    [aleph.tcp :as tcp]
    [taoensso.timbre :as log]
    [gloss.core :as gloss]
    [gloss.io :as io]
    [cheshire.core :as cheshire])
   (:gen-class))

(def protocol
  (gloss/compile-frame
   (gloss/string :utf-8)
   cheshire/generate-string
   cheshire/parse-string))

(defn handle-message
  [msg]
  (log/info "Got message" (String. msg)))

(defn handler
  [s info]
  (log/info "New connection" info)
  (d/chain
   (s/take! s)
   (fn [msg]
     ;; Close the connection after receiving a message.
     (s/close! s)
     (d/future (handle-message msg)))))

(defn start-server
  [options]
  (tcp/start-server handler options))

(defn -main
  [& args]
  (println "Node"))
