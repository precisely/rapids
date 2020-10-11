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
    (let [run (start-flow! suspending-flow :foo)]
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])

      (testing "send event"
        (let [ev-result (simulate-event! run :test-event)]
          (is (instance? Run ev-result))
          (is-log [:before-suspend :after-suspend])

          (testing "it returns the correct value"
            (is (= (:result ev-result) :foo)))))))

  (testing "suspend! event provides a value"
    (let [run (simulate-event! (start-flow! event-value-flow :foo) :foo "foo-result")]
      (is (= (:result run) "foo-result"))))

  (testing "providing a mismatched event-id throws an exception"
    (is (thrown? Exception (simulate-event! (start-flow! event-value-flow :expecting) :actual)))))

(deflow fl-nest [arg event-id]
  (* arg (suspend! event-id)))

(deflow nested-flow-args [a]
  (fl-nest (fl-nest (fl-nest a :first) :second) :third))

(defn a [n] (* n 10))
(deflow fl-alternating []
  (+ (suspend! :first-arg) (a 2) (fl-nest 100 :third)))

(deflow fl-keywords [& {:keys [a b c]}]
  (+ a b c (suspend! :event)))

(deftest ^:unit FunctionalExpressionTest
  (testing "nested flow arguments"
    (let [run (start-flow! nested-flow-args 2)
          run2 (simulate-event! run :first 3)
          run3 (simulate-event! run2 :second 5)
          run4 (simulate-event! run3 :third 7)]
      (is (= (:result run4) (* 2 3 5 7)))))

  (testing "various suspending and non-suspending args"
    (let [run (simulate-event! (simulate-event! (start-flow! fl-alternating) :first-arg 1) :third 3)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 321))))

  (testing "accepts keywords"
    (let [run (simulate-event! (start-flow! fl-keywords :a 1 :b 10 :c 100) :event 1000)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 1111)))))

(deflow conditional-suspend [test]
  (if test
    (suspend! :then)
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:unit SimpleConditionals
  (testing "conditional suspends in then expr"
    (clear-log!)
    (let [run (start-flow! conditional-suspend true)]
      (testing "before event")
      (is (instance? Run run))
      (is-log [])
      (testing "after event"
        (let [run (simulate-event! run :then)]
          (is (= (:result run) :final-value))
          (is-log [:done])))))

  (testing "but conditional does not suspend in else expr"
    (clear-log!)
    (let [run (start-flow! conditional-suspend false)]
      (is (= (:result run) :final-value))
      (is-log [:else :done]))))

(deflow nested-conditional-suspend [level1 level2]
  (if level1
    (if level2
      (suspend! :true-true)
      (log! :true-false))
    (if level2
      (log! :false-true)
      (suspend! :false-false)))
  (log! :done))

(deftest ^:unit NestedConditionals
  (testing "suspend! correctly suspends inside nested then"
    (clear-log!)
    (let [run (start-flow! nested-conditional-suspend true true)]

      (is (run-in-state? run :suspended))
      (let [run-after-event (simulate-event! run :true-true)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done]))))

  (testing "non-suspending expression in nested else returns immediately"
    (clear-log!)
    (let [run (start-flow! nested-conditional-suspend true false)]
      (is (run-in-state? run :complete))
      (is-log [:true-false :done])))

  (testing "non-suspending expression in nested then returns immediately"
    (clear-log!)
    (let [run (start-flow! nested-conditional-suspend false true)]
      (is (run-in-state? run :complete))
      (is-log [:false-true :done])))

  (testing "suspend! correctly suspends inside nested else"
    (clear-log!)
    (let [run (start-flow! nested-conditional-suspend false false)]

      (is (run-in-state? run :suspended))
      (let [run-after-event (simulate-event! run :false-false)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done])))))

(deflow conditional-with-suspending-test [val]
  (if (suspending-flow val)
    :then-val
    :else-val))

(deftest ^:unit SuspendingTest
  (testing "if expression where the test suspends, "
    (testing "when test is truthy"
      (let [run (start-flow! conditional-with-suspending-test true)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :suspended)))

        (let [run (simulate-event! run :test-event)]
          (testing "it should return the then expression value"
            (is (= (:result run) :then-val))))))

    (testing "when test is falsey"
      (let [run (start-flow! conditional-with-suspending-test false)]
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
    (let [run (start-flow! non-suspending-let-flow 2)]
      (is (= (-> run :result) [2 3 6]))))

  (testing "correctly binds a suspending initial value"
    (let [run (simulate-event! (start-flow! suspending-let-initial-binding-flow 3) :initial-binding "event-data")]
      (is (= (:result run) ["event-data", 9]))))

  (testing "correctly binds a suspending internal value"
    (let [run (simulate-event! (start-flow! suspending-let-internal-binding-flow 3) :internal-binding "event-data")]
      (is (= (:result run) [9, "event-data", 27]))))

  (testing "correctly binds a suspending final value"
    (let [run (simulate-event! (start-flow! suspending-let-final-binding-flow 3) :final-binding "event-data")]
      (is (= (:result run) [9, 27, "event-data"]))))

  (testing "correctly handles body with suspending value"
    (let [run (simulate-event! (start-flow! suspending-let-body-flow 3) :body "body-event-data")]
      (is (= (:result run) [9, 27, "body-event-data"])))))

(deflow simple-loop [n]
  (log! :before-suspend)
  (suspend! :initial-wait)
  (log! :after-suspend)
  (loop [a 1]
    (log! :looped)
    (if (< a n)
      (recur (+ a 1))
      n)))

(deflow loop-with-suspending-body []
  (log! :before-loop)
  (loop [a 1]
    (log! :inside-loop)
    (if (suspend! :continue-loop)
      (recur (+ a 1))
      a)))

(deftest ^:unit LoopTest
  (testing "simple loop with no suspending operations works like a normal loop"
    (clear-log!)
    (let [run (start-flow! simple-loop 3)]
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])
      (let [run (simulate-event! run :initial-wait)]
        (is (run-in-state? run :complete))
        (is-log [:before-suspend :after-suspend :looped :looped :looped])
        (is (:result run) 3))))

  (testing "loop with suspend in body"
    (testing "single iteration"
      (clear-log!)
      (let [run (start-flow! loop-with-suspending-body)]
        (is (run-in-state? run :suspended))
        (let [run (simulate-event! run :continue-loop false)]
          (is (run-in-state? run :complete))
          (is-log [:before-loop :inside-loop])
          (is (= (:result run) 1)))))

    (testing "multiple iterations"
      (clear-log!)
      (let [run (start-flow! loop-with-suspending-body)
            run (simulate-event! run :continue-loop true)             ; a + 1 = 2
            run (simulate-event! run :continue-loop true)             ; a + 1 = 3
            run (simulate-event! run :continue-loop false)]           ; a + 1 = 4
        (is (run-in-state? run :complete))
        (is (:result run) 4)
        (is-log [:before-loop :inside-loop :inside-loop :inside-loop])))))