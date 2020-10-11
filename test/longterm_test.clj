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

(defn simulate-event!
  ([run event-id]
   (simulate-event! run event-id nil))
  ([run event-id value]
   {:pre [(instance? Run run)]}
   (process-event! {:event-id event-id :run-id (:id run) :data value})))

(deflow suspending-flow
  [val]
  (log! :before-suspend)
  (suspend! :test-event)
  (log! :after-suspend)
  val)

(deflow event-value-flow
  "Just returns an event value"
  [event-id] (suspend! event-id))

(deftest ^:unit BasicFlowTests
  (testing "Start and suspend"
    (clear-log!)
    (let [run (start-run! suspending-flow :foo)]
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])

      (testing "send event"
        (let [ev-result (simulate-event! run :test-event)]
          (is (instance? Run ev-result))
          (is-log [:before-suspend :after-suspend])

          (testing "it returns the correct value"
            (is (= (:result ev-result) :foo)))))))

  (testing "suspend! event provides a value"
    (let [run (simulate-event! (start-run! event-value-flow :foo) :foo "foo-result")]
      (is (= (:result run) "foo-result")))))

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
        (let [run (simulate-event! run :then)]
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
      (let [run-after-event (simulate-event! run :then-else)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done]))))

  (testing "suspend! correctly suspends inside nested else"
    (clear-log!)
    (let [run (start-run! nested-conditional-suspend false false false)]

      (is (run-in-state? run :suspended))
      (let [run-after-event (simulate-event! run :then-else)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done])))))

(deflow conditional-with-suspending-test [val]
  (if (suspending-flow val)
    :then-val
    :else-val))

(deftest ^:unit SuspendingTest
  (testing "if expression where the test suspends, "
    (testing "when test is truthy"
      (let [run (start-run! conditional-with-suspending-test true)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :suspended)))

        (let [run (simulate-event! run :test-event)]
          (testing "it should return the then expression value"
            (is (= (:result run) :then-val))))))

    (testing "when test is falsey"
      (let [run (start-run! conditional-with-suspending-test false)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :suspended)))

        (let [run (simulate-event! run :test-event)]
          (testing "it should return the else expression value"
            (is (= (:result run) :else-val))))))))

(deflow non-suspending-let-flow [a]
  (let [b (+ 1 a)
        c (* b a)]
    (if false (suspend! :foo))                              ; satisfy deflow suspend requirement but do nothing
    [a b c]))

(deflow suspending-let-initial-binding-flow [arg]
  (let [suspend-value (suspend! :initial-binding)
        square (* arg arg)]
    [suspend-value square]))

(deflow suspending-let-internal-binding-flow [arg]
  (let [square (* arg arg)
        suspend-value (suspend! :internal-binding)
        cube (* arg arg arg)]
    [square suspend-value cube]))

(deflow suspending-let-final-binding-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)
        suspend-value (suspend! :final-binding)]
    [square cube suspend-value]))

(deflow suspending-let-body-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)]
    [square cube (suspend! :body)]))

(deftest ^:unit LetTest
  (testing "non-suspending let expressions work"
    (let [run (start-run! non-suspending-let-flow 2)]
      (is (= (-> run :result) [2 3 6]))))

  (testing "correctly binds a suspending initial value"
    (let [run (simulate-event! (start-run! suspending-let-initial-binding-flow 3) :initial-binding "event-data")]
      (is (= (:result run) ["event-data", 9]))))

  (testing "correctly binds a suspending internal value"
    (let [run (simulate-event! (start-run! suspending-let-internal-binding-flow 3) :internal-binding "event-data")]
      (is (= (:result run) [9, "event-data", 27]))))

  (testing "correctly binds a suspending final value"
    (let [run (simulate-event! (start-run! suspending-let-final-binding-flow 3) :final-binding "event-data")]
      (is (= (:result run) [9, 27, "event-data"]))))

  (testing "correctly handles body with suspending value"
    (let [run (simulate-event! (start-run! suspending-let-body-flow 3) :body "body-event-data")]
      (is (= (:result run) [9, 27, "body-event-data"])))))


