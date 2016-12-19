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

(defn conj-nodes-and-me
  [nodes me]
  (sort
   (conj nodes me)))

(defn initial-state
  [me nodes]
  {:critical-section? false
   :me me
   :nodes nodes
   :requests (nodes->requests (conj-nodes-and-me nodes me))})

(defn initial-token
  [me nodes]
  {:last-requests (nodes->requests (conj-nodes-and-me nodes me))
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
  [me nodes]
  (assoc (initial-state me nodes)
         :token (initial-token me nodes)))

(defn me-request-number
  [state]
  (get-in state [:requests (:me state)]))

(defn send-request
  [state]
  (fn [send-fn]
    (doseq [node (:nodes state)]
      (send-fn node
               (construct-request-msg
                :sender (:me state)
                :request-number (me-request-number state))))))

(defn inc-me-request-number
  [state]
  (let [sender (:me state)]
    (update-in state [:requests sender] inc)))

(defn request-critical-section
  [state]
  (log/info "Request critical section" state)
  (let [new-state (inc-me-request-number state)]
    {:state new-state
     :action (send-request new-state)}))

(defn enter-critical-section
  [state]
  (log/info "Enter critical section" state)
  ;; Check if can be entered?
  {:state (assoc state :critical-section? true)
   :action (fn [_ &])})

(defn request-number-from-state-to-token
  [state]
  (assoc-in state
            [:token :last-requests (:me state)]
            (me-request-number state)))

(defn outstanding-request?
  ([state id]
   (outstanding-request? (:requests state)
                         (get-in state [:token :last-requests])
                         id))
  ([requests last-requests id]
   (= (get requests id)
      (inc (get last-requests id)))))

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
                :sender (:me state)
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
               (construct-token-msg :sender (:me state)
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

(defn has-token?
  [state]
  (contains? state :token))

(defn in-critical-section?
  [state]
  (:critical-section? state))

(defn can-execute-critical-section?
  [state]
  (and (has-token? state)
       (not (in-critical-section? state))))
