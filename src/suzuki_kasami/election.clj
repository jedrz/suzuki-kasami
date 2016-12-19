(ns suzuki-kasami.election
  (:require [taoensso.timbre :as log]))

(def broadcast-msg-type "electionBroadcast")
(def ok-msg-type "electionOk")

(defn construct-broadcast-msg
  [& {:keys [sender]}]
  {"senderId" sender
   "type" broadcast-msg-type
   "value" {"nodeId" sender}})

(defn broadcast-msg?
  [msg]
  (= (get msg "type") broadcast-msg-type))

(defn construct-ok-msg
  [& {:keys [sender]}]
  {"senderId" sender
   "type" ok-msg-type})

(defn ok-msg?
  [msg]
  (= (get msg "type") ok-msg-type))

(defn election-msg?
  [msg]
  ((some-fn broadcast-msg? ok-msg?) msg))

(defn initial-state
  [me nodes]
  {:finished? false
   :elected? false
   :me me
   :confirmations #{}
   :nodes (into #{} nodes)})

(defn not-confirmed
  [state]
  (filter (complement (:confirmations state))
          (:nodes state)))

(defn send-broadcast
  [state]
  (fn [send-fn]
    (doseq [node (not-confirmed state)]
      (send-fn node
               (construct-broadcast-msg :sender (:me state))))))

(defn extract-sender
  [msg]
  (get msg "senderId"))

(defn send-ok
  [state msg]
  (fn [send-fn]
    (send-fn (extract-sender msg)
             (construct-ok-msg :sender (:me state)))))

(defn start-election
  [state]
  (log/info "Start election")
  {:state state
   :action (send-broadcast state)})

(defn other-stronger?
  [state msg]
  (> (extract-sender msg)
     (:me state)))

(defn respond-ok
  [state msg]
  (log/info "Respond ok")
  {:state (assoc state :finished? true)
   :action (send-ok state msg)})

(defn handle-broadcast
  [state msg]
  (log/info "Handle broadcast" state msg)
  (if (other-stronger? state msg)
    (respond-ok state msg)
    (start-election state)))

(defn extend-confirmations
  [state msg]
  (log/info "Extend confirmations" state msg)
  (merge-with into state {:confirmations #{(extract-sender msg)}}))

(defn update-elected
  [state msg]
  (log/info "Update elected" state msg)
  (assoc state
         :elected? (= (:confirmations state) (:nodes state))
         :finished? true))

(defn handle-ok
  [state msg]
  (log/info "Handle ok" state msg)
  (let [new-state (-> state
                      (extend-confirmations msg)
                      (update-elected msg))]
    {:state new-state
     :action (fn [& _])}))

(defn choose-handle-fn
  [msg]
  (cond
    (broadcast-msg? msg) handle-broadcast
    (ok-msg? msg) handle-ok))

(defn handle-message
  [state msg]
  ((choose-handle-fn msg) state msg))

(defn elected?
  [state]
  (:elected? state))

(defn finished?
  [state]
  (:finished? state))
