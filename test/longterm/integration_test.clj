(ns longterm.integration_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]
            [longterm.foo :refer :all])
  (:import (longterm.runstore Run)))

(def ^:dynamic *log* (atom []))
(defn clear-log!
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
  (suspend! :test-event)
  (log! :after-suspend))

(deftest ^:unit BasicFlowTests
  (testing "Start and suspend"
    (clear-log!)
    (let [run (start-run! suspending-flow)]
      (is (instance? Run run))
      (is-log [:before-suspend])

      (testing "processing event"
        (let [pe (process-event! {:event-id :test-event :run-id (:id run)})]
          (is (instance? Run pe))
          (is-log [:before-suspend :after-suspend]))))))

(deflow conditional-suspend [test]
  (if test
    (suspend! :then)
    :else)
  (log! :done)
  :final-value)

(deftest ^:unit IfExpressions
  (testing "conditional branch suspends"
    (clear-log!)
    (let [run (start-run! conditional-suspend true)]
      (testing "before event")
      (is (instance? Run run))
      (is-log [])
      (testing "after event"
        (let [run (process-event! {:event-id :then :run-id (:id run)})]
          (is (= (:result run) :final-value))
          (is-log [:done])))))

  (testing "conditional branch continues"
    (clear-log!)
    (let [run (start-run! conditional-suspend false)]
      (is (= (:result run) :final-value))
      (is-log [:done]))))