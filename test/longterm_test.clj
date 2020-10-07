(ns longterm_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]
            [longterm.in-memory-runstore :refer [in-memory-runstore?]])
  (:import (longterm.runstore Run)))

(deftest ^:unit RunStore
  (testing "runstore is set to default InMemoryRunStore"
    (is (in-memory-runstore? @longterm.runstore/runstore))))

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
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])

      (testing "processing event"
        (let [pe (process-event! {:event-id :test-event :run-id (:id run)})]
          (is (instance? Run pe))
          (is-log [:before-suspend :after-suspend]))))))

(deflow conditional-suspend [test]
  (if test
    (suspend! :then)
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:unit SimpleConditionals
  (testing "conditional suspends in then expr"
    (clear-log!)
    (let [run (start-run! conditional-suspend true)]
      (testing "before event")
      (is (instance? Run run))
      (is-log [])
      (testing "after event"
        (let [run (process-event! {:event-id :then :run-id (:id run)})]
          (is (= (:result run) :final-value))
          (is-log [:done])))))

  (testing "but conditional does not suspend in else expr"
    (clear-log!)
    (let [run (start-run! conditional-suspend false)]
      (is (= (:result run) :final-value))
      (is-log [:else :done]))))

(deflow nested-conditional-suspend [test then-test else-test]
  (if test
    (if then-test
      (suspend! :then-else)
      (log! :else-else))
    (if else-test
      (log! :else-then)
      (suspend! :else-else)))
  (log! :done))

(deftest ^:unit NestedConditionals
  (testing "suspend! correctly suspends inside nested then"
    (clear-log!)
    (let [run (start-run! nested-conditional-suspend true true false)
          run-id (:id run)]

      (is (run-in-state? run :suspended))
      (let [run-after-process-event (process-event! {:event-id :then-else :run-id run-id})]
        (is (run-in-state? run-after-process-event :complete))
        (is-log [:done]))))

  (testing "suspend! correctly suspends inside nested else"
    (clear-log!)
    (let [run (start-run! nested-conditional-suspend false false false)
          run-id (:id run)]

      (is (run-in-state? run :suspended))
      (let [run-after-process-event (process-event! {:event-id :then-else :run-id run-id})]
        (is (run-in-state? run-after-process-event :complete))
        (is-log [:done])))))


