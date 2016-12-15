(ns suzuki-kasami.node
  (:require
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [aleph.tcp :as tcp]
   [taoensso.timbre :as log]
   [gloss.core :as gloss]
   [gloss.io :as io]
   [cheshire.core :as cheshire]
   [aleph.netty :as netty])
  (:gen-class))

(def configuration (atom {}))

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
  (log/info "Starting tcp server with options" options)
  (tcp/start-server handler options))

(defn- start-server-and-wait
  [configuration]
  (let [server (start-server (select-keys configuration [:port]))]
    (netty/wait-for-close server)))

(defn -main
  [& args]
  (log/info "Starting node with configuration" args)
  (let [conf-path (first args)
        conf (clojure.edn/read-string (slurp conf-path))]
    (log/info "Read configuration" conf)
    (reset! configuration conf)
    (start-server-and-wait conf)))
