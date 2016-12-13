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
  (log/info "Got message" msg))

(defn message-handler
  [s]
  (d/chain
   (s/take! s)
   (fn [msg]
     ;; Close the connection after receiving a message.
     (log/info "Hmm")
     (s/close! s)
     (d/future (handle-message msg)))))

(defn wrap-with-protocol
  [s]
  (let [out (s/stream)]
    (s/connect
     (s/map #(io/encode protocol %) out)
     s)
    (s/splice
     out
     (io/decode-stream s protocol))))

(defn handler
  [s info]
  (log/info "New connection" info)
  (message-handler (wrap-with-protocol s)))

(defn client
  [& {:keys [host port]}]
  (d/chain
   (tcp/client {:host host :port port})
   wrap-with-protocol))

(defn start-server
  [options]
  (tcp/start-server handler options))

(defn -main
  [& args]
  (println "Node"))
