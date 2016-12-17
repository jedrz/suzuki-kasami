(ns suzuki-kasami.suzuki-kasami)

(defn construct-request-msg
  [& {:keys [sender request-number]}]
  {"senderId" sender
   "type" "request"
   "value" {"requestNumber" request-number}})

