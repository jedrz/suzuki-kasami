(ns suzuki-kasami.resource-server
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
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

(def bad-request {:status 400})
(def ok {:status 200})

(defn set-resource-handler
  [req]
  (let [new-value (get-in req [:body "value"])]
    (if (integer? new-value)
      (do
        (log/info "Set value to" new-value)
        (reset! resource new-value)
        ok)
      bad-request)))

(defroutes routes
  (GET "/" [] read-resource-handler)
  (POST "/" [] set-resource-handler)
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
