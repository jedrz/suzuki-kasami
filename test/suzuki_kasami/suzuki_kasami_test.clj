(ns suzuki-kasami.suzuki-kasami-test
  (:require [suzuki-kasami.suzuki-kasami :as sut]
            [clojure.test :refer :all]))

(deftest messages
  (testing "constructing request message"
    (is (= (sut/construct-request-msg :sender 5 :request-number 20)
           {"senderId" 5
            "type" "request"
            "value" {"requestNumber" 20}}))))
