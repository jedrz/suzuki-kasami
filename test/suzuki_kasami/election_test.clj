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

(deftest handle-msg

  (testing "choosing handle fn for broadcast"
    (let [msg (sut/construct-broadcast-msg :sender 1 :node 2)]
      (is (= (sut/choose-handle-fn msg)
             sut/handle-broadcast))))

  (testing "choosing handle fn for ok"
    (let [msg (sut/construct-ok-msg :sender 1 :node 2)]
      (is (= (sut/choose-handle-fn msg)
             sut/handle-ok)))))

