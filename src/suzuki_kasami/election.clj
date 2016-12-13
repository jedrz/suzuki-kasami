(ns suzuki-kasami.election)

(def broadcast-msg-type "electionBroadcast")
(def ok-msg-type "electionOk")

(defn construct-broadcast-msg
  [& {:keys [sender node]}]
  {"senderId" sender
   "type" broadcast-msg-type
   "value" {"nodeId" node}})

(defn construct-ok-msg
  [& {:keys [sender]}]
  {"senderId" sender
   "type" ok-msg-type})
