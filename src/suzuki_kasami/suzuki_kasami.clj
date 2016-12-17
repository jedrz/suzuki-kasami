(ns suzuki-kasami.suzuki-kasami)

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

(defn initial-state
  [sender nodes]
  {:critical-section? false
   :sender sender
   :nodes nodes
   :requests (nodes->requests nodes)})

(defn initial-token
  [nodes]
  {:last-requests (nodes->requests nodes)
   :queue []})

(defn initial-state-with-token
  [sender nodes token]
  (assoc (initial-state sender nodes)
         :token (initial-token nodes)))
