(ns suzuki-kasami.node-test
  (:require [suzuki-kasami.node :as sut]
            [clojure.test :refer :all]
            [manifold.stream :as s]
            [manifold.deferred :as d]))

(deftest protocol

  (testing "encoding"
    (is (= (-> sut/protocol
               (gloss.io/encode {:value 1})
               (byte-streams/convert String))
           "{\"value\":1}")))

  (testing "decoding"
    (let [value-in-bytes (byte-streams/to-byte-array "{\"value\":1}")]
      (is (= (gloss.io/decode sut/protocol value-in-bytes)
             {"value" 1})))))
