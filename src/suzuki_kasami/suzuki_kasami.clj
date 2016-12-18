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

(defn sender-from-request
  [msg]
  (get msg "senderId"))

(defn request-number-from-msg
  [msg]
  (get-in msg ["value" "requestNumber"]))

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

(defn requests->unmessaged
  [requests]
  (->> requests
       (map (fn [entry]
              [(get entry "nodeId")
               (get entry "number")]))
       (into {})))

(defn token-from-msg
  [msg]
  {:last-requests (requests->unmessaged (get-in msg ["value" "lastRequests"]))
   :queue (into [] (get-in msg ["value" "queue"]))})

(defn initial-state-with-token
  [sender nodes]
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

(defn enter-critical-section
  [state]
  (log/info "Enter critical section" state)
  ;; Check if can be entered?
  {:state (assoc state :critical-section? true)
   :action (fn [_ &])})

(declare request-number-from-state)

(defn request-number-from-state-to-token
  [state]
  (assoc-in state
            [:token :last-requests state]
            (request-number-from-state state (:sender state))))

(declare outstanding-request?)

(defn update-queue-itself
  [queue requests last-requests]
  (let [all-nodes (keys requests)
        not-in-queue (clojure.set/difference (set all-nodes) (set queue))]
    (reduce (fn [q n]
              (if (outstanding-request? requests last-requests n)
                (conj q n)
                q))
            queue
            not-in-queue)))

(defn update-queue
  [state]
  (update-in state
             [:token :queue]
             #(update-queue-itself %
                                   (:requests state)
                                   (get-in state [:token :last-requests]))))

(defn mark-cs-left
  [state]
  (assoc state :critical-section? false))

(defn first-from-queue
  [state]
  (get-in state [:token :queue 0]))

(defn drop-first-from-queue
  [state]
  (update-in state
             [:token :queue]
             #(into [] (rest %))))

(defn maybe-dissoc-token
  [state new-owner]
  (if (nil? new-owner)
    state
    (dissoc state :token)))

(defn maybe-release-token-after-cs
  [state new-owner]
  (fn [send-fn]
    (when-not (nil? new-owner)
      (log/info "Releasing token after critical section to" new-owner)
      (send-fn new-owner
               (construct-token-msg
                :sender (:sender state)
                :token (:token state))))))

(defn release-critical-section
  [state]
  (log/info "Release critical section" state)
  (let [new-state (-> state
                      request-number-from-state-to-token
                      update-queue
                      mark-cs-left)
        new-token-owner (first-from-queue new-state)
        state-with-shorter-queue (drop-first-from-queue new-state)]
    {:state (maybe-dissoc-token state-with-shorter-queue new-token-owner)
     :action (maybe-release-token-after-cs state-with-shorter-queue
                                           new-token-owner)}))

(defn update-request-number
  [state msg]
  (let [msg-sender (sender-from-request msg)
        number-from-msg (request-number-from-msg msg)]
    (update-in state
               [:requests msg-sender]
               #(max % number-from-msg))))

(defn request-number-from-state
  [state id]
  (get-in state [:requests id]))

(defn request-number-from-token
  [token id]
  (get-in token [:last-requests id]))

(defn outstanding-request?
  ([state id]
   (outstanding-request? (:requests state)
                         (get-in state [:token :last-requests])
                         id))
  ([requests last-requests id]
   (= (get requests id)
      (inc (get last-requests id)))))

(defn release-token-on-request?
  [state msg]
  (and
   (contains? state :token)
   (not (:critical-section? state))
   (outstanding-request? state (sender-from-request msg))))

(defn maybe-dissoc-token-on-request
  [state msg]
  (if (release-token-on-request? state msg)
    (dissoc state :token)
    state))

(defn maybe-release-token-on-request
  [state request-msg]
  (fn [send-fn]
    (when (release-token-on-request? state request-msg)
      (log/info "Releasing token on request" request-msg)
      (send-fn (sender-from-request request-msg)
               (construct-token-msg :sender (:sender state)
                                    :token (:token state))))))

(defn handle-request
  [state msg]
  (log/info "Handle request" state msg)
  (let [new-state (update-request-number state msg)]
    {:state (maybe-dissoc-token-on-request new-state msg)
     :action (maybe-release-token-on-request new-state msg)}))

(defn handle-token
  [state msg]
  (log/info "Handle token" state msg)
  {:state (assoc state :token (token-from-msg msg))
   :action (fn [& _])})

(defn choose-handle-fn
  [msg]
  (cond
    (request-msg? msg) handle-request
    (token-msg? msg) handle-token))

(defn handle-message
  [state msg]
  ((choose-handle-fn msg) state msg))
