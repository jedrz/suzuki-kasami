(ns suzuki-kasami.node
  (:require
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [aleph.tcp :as tcp]
   [taoensso.timbre :as log]
   [gloss.core :as gloss]
   [gloss.io :as io]
   [cheshire.core :as cheshire]
   [aleph.netty :as netty]
   [aleph.http :as http]
   [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
   [compojure.core :refer [defroutes POST]]
   [suzuki-kasami.election :as election])
  (:gen-class))

(declare client)

(defn extract-ids
  [c]
  (map :id (:nodes c)))

(def configuration)

(def election-state (ref {}))

(def suzuki-kasami-state (ref {}))

(defn create-client
  [id]
  (let [node (first (filter #(= (:id %) id) (:nodes configuration)))]
    (client :host (:host node) :port (:port node))))

(defn send-fn
  [id msg]
  (log/info "Sending to node" id "msg" msg)
  @(d/chain
    (create-client id)
    (fn [s]
      (s/put! s msg)
      (s/close! s))))

(defn handle-election
  [election-fn]
  (let [action
        (dosync
         (let [{:keys [state action]} (election-fn @election-state)]
           (ref-set election-state state)
           action))]
    (log/info "New election state" @election-state)
    (action send-fn)))

(defn handle-message
  [msg]
  (log/info "Got message" msg)
  (cond
    (election/election-msg? msg) (handle-election
                                  #(election/handle-message % msg))))

(defn start-election
  []
  (log/info "Staring election")
  (handle-election election/start-election)
  {:status 200})

(defn modify-resource
  [value]
  (log/info "Modifying resource with value" value)
  ;; TODO:
  {:status 200})

(defn message-handler
  [s]
  (d/chain
   (s/take! s)
   (fn [msg]
     ;; Close the connection after receiving a message.
     (s/close! s)
     (d/future (handle-message msg)))))

(def protocol
  (gloss/compile-frame
   (gloss/string :utf-8)
   cheshire/generate-string
   cheshire/parse-string))

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

(defroutes admin-routes
  (POST "/election" [] (fn [req] (start-election)))
  (POST "/resource" [] (fn [req] (modify-resource
                                  (get-in req [:body "value"] 0)))))

(def admin-handler
  (-> admin-routes
      wrap-json-response
      wrap-json-body))

(defn start-admin-server
  [configuration]
  (let [port (:admin-port configuration)]
    (log/info "Starting admin server on port" port)
    (http/start-server admin-handler {:port port})))

(defn -main
  [& args]
  (log/info "Starting node with configuration" args)
  (let [conf-path (first args)
        conf (clojure.edn/read-string (slurp conf-path))]
    (log/info "Read configuration" conf)
    (alter-var-root #'configuration (fn [& args] conf))
    (dosync
     (ref-set election-state
              (election/initial-state (:id conf) (extract-ids conf))))
    (start-admin-server conf)
    (start-server-and-wait conf)))
