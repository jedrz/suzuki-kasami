(ns suzuki-kasami.node-test-with-server
  (:require [suzuki-kasami.node :as sut]
            [clojure.test :refer :all]))

(alter-var-root #'*load-tests* (constantly false))

(def port 10001)

(defn with-server
  [f]
  (with-open [s (sut/start-server {:port port})]
    (f)))

(use-fixtures :each with-server)

(defn client
    []
    (sut/client :host "localhost" :port port))

(deftest server

  (defn no-op-handle-message [msg])

  (with-redefs [sut/handle-message no-op-handle-message]

    (testing "connection is opened after establishing connection"
      (is (= (s/closed? @(client)) false)))

    (testing "closing the connection after receiving a message"
      (let [c @(client)]

        @(s/put! c {:value 1})

        (is (= (s/closed? c) true)))))

  (testing "delivering message"
    (let [received-msg (atom nil)]
      (with-redefs [sut/handle-message (fn [msg] (reset! received-msg msg))]
        (let [expected-msg {"value" 1}
              c @(client)]

          @(s/put! c expected-msg)

          (is (= @received-msg expected-msg)))))))

(alter-var-root #'*load-tests* (constantly true))
