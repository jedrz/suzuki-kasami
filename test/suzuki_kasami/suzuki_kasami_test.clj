(ns suzuki-kasami.suzuki-kasami-test
  (:require [suzuki-kasami.suzuki-kasami :as sut]
            [clojure.test :refer :all]))

(deftest messages
  (testing "constructing request message"
    (is (= (sut/construct-request-msg :sender 5 :request-number 20)
           {"senderId" 5
            "type" "request"
            "value" {"requestNumber" 20}})))

  (testing "constructing token message"
    (let [nodes [2 3 4]
          token (-> (sut/initial-token 1 nodes)
                    (assoc :queue [2])
                    (assoc-in [:last-requests 3] 1))]
      (is (= (sut/construct-token-msg :sender 5 :token token)
             {"senderId" 5
              "type" "token"
              "value" {"lastRequests" [{"nodeId" 1
                                        "number" 0}
                                       {"nodeId" 2
                                        "number" 0}
                                       {"nodeId" 3
                                        "number" 1}
                                       {"nodeId" 4
                                        "number" 0}]
                       "queue" [2]}})))))

(deftest state
  (testing "initial state"
    (is (= (sut/initial-state 1 [2 3 4])
           {:critical-section? false
            :sender 1
            :nodes [2 3 4]
            :requests {1 0, 2 0, 3 0, 4 0}})))

  (testing "initial token"
    (is (= (sut/initial-token 1 [2 3])
           {:last-requests {1 0, 2 0, 3 0}
            :queue []})))

  (testing "initial state with token"
    (let [nodes [2 3 4]
          token (sut/initial-token 1 nodes)]
      (is (= (sut/initial-state-with-token 1 nodes)
             {:critical-section? false
              :sender 1
              :nodes nodes
              :requests {1 0, 2 0, 3 0, 4 0}
              :token token})))))

(deftest protocol
  (testing "incrementing sender's request number"
    (let [state (sut/initial-state 2 [1 3 4])
          inc3 (nth (iterate sut/inc-sender-request-number state) 2)]
      ;; Increment second node.
      (is (= (get (:requests inc3) 2)
             2))
      ;; Don't modify other.
      (is (= (get (:requests inc3) 1)
             0))))

  (testing "getting sender's request number"
    (let [state (sut/initial-state 2 [1 3 4])
          inc-state (sut/inc-sender-request-number
                     (sut/inc-sender-request-number state))]
      (is (= (sut/sender-request-number inc-state)
             2))))

  (testing "updating request number from message"
    (let [state (sut/initial-state 2 [1 3 4])
          msg (sut/construct-request-msg :sender 3 :request-number 4)
          new-state (sut/update-request-number state msg)]
      (is (= (get-in new-state [:requests 3])
             4))))

  (testing "ignoring update request number from message if lower than in state"
    ;; TODO:
    ))
