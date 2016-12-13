(ns suzuki-kasami.election-test
  (:require [suzuki-kasami.election :as sut]
            [clojure.test :refer :all]))

(deftest messages
  (testing "constructing broadcast message"
    (is (= (sut/construct-broadcast-msg :sender 5 :node 20)
           {"senderId" 5
            "type" "electionBroadcast"
            "value" {"nodeId" 20}})))

  (testing "constructing ok message"
    (is (= (sut/construct-ok-msg :sender 5)
           {"senderId" 5
            "type" "electionOk"}))))

