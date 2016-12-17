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
          token (-> (sut/initial-token nodes)
                    (assoc :queue [2])
                    (assoc-in [:last-requests 3] 1))]
      (is (= (sut/construct-token-msg :sender 5 :token token)
             {"senderId" 5
              "type" "token"
              "value" {"lastRequests" [{"nodeId" 2
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
            :requests {2 0, 3 0, 4 0}})))

  (testing "initial state with token"
    (let [nodes [2 3 4]
          token (sut/initial-token nodes)]
      (is (= (sut/initial-state-with-token 1 nodes token)
             {:critical-section? false
              :sender 1
              :nodes nodes
              :requests {2 0, 3 0, 4 0}
              :token token})))))
