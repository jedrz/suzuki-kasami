(ns suzuki-kasami.suzuki-kasami)

(defn construct-request-msg
  [& {:keys [sender request-number]}]
  {"senderId" sender
   "type" "request"
   "value" {"requestNumber" request-number}})

(defn request-msg?
  [msg]
  (= (get msg "type") "request"))

(defn token-msg?
  [msg]
  (= (get msg "type") "token"))

(defn sk-msg?
  [msg]
  ((some-fn request-msg? token-msg?) msg))
