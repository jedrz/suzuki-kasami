(ns suzuki-kasami.resource-server
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.deferred :as d]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [taoensso.timbre :as log]
            [environ.core :refer [env]])
  (:gen-class))

(def resource-value (atom 0))
(def resource-blocked? (atom false))

(defn read-resource-handler
  [req]
  (log/info "Read value")
  (response {:value @resource-value}))

(def ok {:status 200})
(def bad-request {:status 400})
(def conflict {:status 409})

(defn valid-value?
  [value]
  (integer? value))

(defn get-value-from-req
  [req]
  (let [value (get-in req [:body "value"])]
    (when (valid-value? value)
      value)))

(defn set-resource-handler
  [req]
  (let [new-value (get-value-from-req req)]
    (if new-value
      (do
        (log/info "Set value to" new-value)
        (reset! resource-value new-value)
        ok)
      bad-request)))

(defn release-resource
  []
  (reset! resource-blocked? false)
  (log/info "Resource released"))

(defn run-and-release-resource
  [d]
  (log/info "Resource acquired")
  (d/finally d release-resource))

(defn protecting-resource-handler
  [orig-handler]
  (fn [req]
    (log/info "Trying to acquire resource")
    (if (compare-and-set! resource-blocked? false true)
      (run-and-release-resource (orig-handler req))
      (do
        (log/warn "Resource already locked!")
        conflict))))

(defn random-sleep
  []
  (Thread/sleep
   (+ 3000
      (rand-int 5000))))

(defn delaying-handler
  [orig-handler]
  (fn [req]
    (d/future
      (log/info "Delaying")
      (random-sleep)
      (log/info "Finished delaying")
      (orig-handler req))))

(defroutes routes
  (GET "/" [] (protecting-resource-handler read-resource-handler))
  (POST "/" [] (protecting-resource-handler
                (delaying-handler set-resource-handler)))
  (route/not-found "No such page."))

(def handler
  (-> routes
      wrap-json-response
      wrap-json-body))

(defn configure-logging
  []
  (log/merge-config! {:timestamp-opts {:pattern "yy-MM-dd HH:mm:ss:SSS"}}))

(defn -main
  [& args]
  (configure-logging)
  (log/info "Starting resource server")
  (let [port (Integer. (env :port 8080))
        server (http/start-server handler {:port port})]
    (netty/wait-for-close server)))
