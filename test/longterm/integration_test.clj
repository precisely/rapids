(ns longterm.integration_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]))

(def ^:dynamic *log* (atom []))
(defn clear!
  []
  (reset! *log* []))

(defn log!
  [val]
  (reset! *log* (conj @*log* val)))

(defmacro is-log
  [expected]
  `(is (= ~expected @*log*)))

(deflow suspending-flow
  []
  (log! :before-suspend)
  (suspend :test-event)
  (log! :after-suspend))

(deftest ^:unit SystemTest
  (testing "Start and suspend"
    (clear!)
    (let [run (start-run! `(suspending-flow))]
      (is-log [:before-suspend])

      (testing "handling event"
        (let [pe (process-event! {:event-id :test-event :run-id (:id run)})]
          (is-log [:before-suspend :after-suspend]))))))

