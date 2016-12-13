(ns suzuki-kasami.resource-server-test
  (:require [suzuki-kasami.resource-server :as sut]
            [clojure.test :refer :all]))

(deftest responses
  (testing "ok"
    (is (= sut/ok {:status 200}))))
