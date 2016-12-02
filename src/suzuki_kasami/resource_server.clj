(ns suzuki-kasami.resource-server
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.deferred :as d]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [taoensso.timbre :as log])
  (:gen-class))

(def resource (atom 0))

(defn read-resource-handler
  [req]
  (log/info "Read value")
  (response {:value @resource}))

(def ok {:status 200})
(def bad-request {:status 400})
(def confict {:status 409})

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
        (reset! resource new-value)
        ok)
      bad-request)))

(defn delayed-handler
  [orig-handler]
  (fn [req]
    (d/future
      (Thread/sleep 5000)
      (orig-handler req))))

(defroutes routes
  (GET "/" [] read-resource-handler)
  (POST "/" [] (delayed-handler set-resource-handler))
  (route/not-found "No such page."))

(def handler
  (-> routes
      wrap-json-response
      wrap-json-body))

(defn -main
  [& args]
  (log/info "Starting resource server")
  (let [server (http/start-server handler {:port 8080})]
    (netty/wait-for-close server)))
