(ns suzuki-kasami.suzuki-kasami
  (:require [taoensso.timbre :as log]))

(defn construct-request-msg
  [& {:keys [sender request-number]}]
  {"senderId" sender
   "type" "request"
   "value" {"requestNumber" request-number}})

(defn request-msg?
  [msg]
  (= (get msg "type") "request"))

(defn- requests->messaged
  [requests]
  (map (fn [[id number]]
         {"nodeId" id
          "number" number})
       requests))

(defn construct-token-msg
  [& {:keys [sender token]}]
  {"senderId" sender
   "type" "token"
   "value" {"lastRequests" (requests->messaged (:last-requests token))
            "queue" (:queue token)}})

(defn token-msg?
  [msg]
  (= (get msg "type") "token"))

(defn sk-msg?
  [msg]
  ((some-fn request-msg? token-msg?) msg))

(defn- nodes->requests
  [nodes]
  (->> (repeat (count nodes) 0)
       (zipmap nodes)))

(defn conj-nodes-and-sender
  [nodes sender]
  (sort
   (conj nodes sender)))

(defn initial-state
  [sender nodes]
  {:critical-section? false
   :sender sender
   :nodes nodes
   :requests (nodes->requests (conj-nodes-and-sender nodes sender))})

(defn initial-token
  [sender nodes]
  {:last-requests (nodes->requests (conj-nodes-and-sender nodes sender))
   :queue []})

(defn initial-state-with-token
  [sender nodes token]
  (assoc (initial-state sender nodes)
         :token (initial-token sender nodes)))

(defn sender-request-number
  [state]
  (get-in state [:requests (:sender state)]))

(defn send-request
  [state]
  (fn [send-fn]
    (doseq [node (:nodes state)]
      (let [sender (:sender state)]
        (send-fn node
                 (construct-request-msg
                  :sender sender
                  :request-number (sender-request-number state)))))))

(defn inc-sender-request-number
  [state]
  (let [sender (:sender state)]
    (update-in state [:requests sender] inc)))

(defn request-critical-section
  [state]
  (log/info "Request critical section" state)
  (let [new-state (inc-sender-request-number state)]
    {:state new-state
     :action (send-request new-state)}))
