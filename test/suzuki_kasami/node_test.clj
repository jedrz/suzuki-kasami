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

(deftest server

  (def port 10000)

  (defn with-server
    [f]
    (with-open [s (sut/start-server {:port port})]
      (f)))

  (use-fixtures :each with-server)

  (defn client
    []
    (sut/client :host "localhost" :port port))

  (defn no-op-handle-message [msg])

  (with-redefs [sut/handle-message no-op-handle-message]

    (testing "connection is opened after establishing connection"
      (is (= (s/closed? @(client)) false)))

    (testing "closing the connection after receiving a message"
      (let [c @(client)]
        (d/chain
         (s/put! c {:value 1})
         #(is (= (s/closed? c) true)))))))
