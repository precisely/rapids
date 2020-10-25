(ns longterm_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]
            [longterm.in-memory-runstore :refer [in-memory-runstore?]]
            [longterm.runstore :as rs]))

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
  ([run]
   (simulate-event! run nil nil))

  ([run permit]
   (simulate-event! run permit nil))
  ([run permit value]
   {:pre [(run-in-state? run :any)]}
   (continue! (:id run) permit value)))

(deflow suspending-flow
  [val]
  (log! :before-suspend)
  (suspend!)
  (log! :after-suspend)
  val)

(deflow event-value-flow
  "Just returns an event value"
  [permit] (suspend! :permit permit))

(deftest ^:unit BasicFlowTests

  (testing "Start and suspend"
    (clear-log!)
    (let [run (start! suspending-flow :foo)]
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])

      (testing "send event - with no permit"
        (let [ev-result (simulate-event! run)]
          (is (run-in-state? ev-result :complete))
          (is-log [:before-suspend :after-suspend])

          (testing "it returns the correct value"
            (is (= (:result ev-result) :foo)))))))

  (testing "suspend! event provides a value"
    (let [run (simulate-event! (start! event-value-flow :foo) :foo "foo-result")]
      (is (= (:result run) "foo-result"))))

  (testing "providing a mismatched context throws an exception"
    (is (thrown? Exception (simulate-event! (start! event-value-flow :expecting) :actual)))))

(deflow fl-nest [arg context]
  (* arg (suspend! :permit context)))

(deflow nested-flow-args [a]
  (fl-nest (fl-nest (fl-nest a :first) :second) :third))

(defn a [n] (* n 10))
(deflow fl-alternating []
  (+ (suspend! :permit :first-arg) (a 2) (fl-nest 100 :third)))

(deflow fl-keywords [& {:keys [a b c]}]
  (+ a b c (suspend! :permit :event)))

(deftest ^:unit FunctionalExpressionTest
  (testing "nested flow arguments"
    (let [run  (start! nested-flow-args 2)
          run2 (simulate-event! run :first 3)
          run3 (simulate-event! run2 :second 5)
          run4 (simulate-event! run3 :third 7)]
      (is (= (:result run4) (* 2 3 5 7)))))
  (testing "various suspending and non-suspending args"
    (let [run (simulate-event! (simulate-event! (start! fl-alternating) :first-arg 1) :third 3)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 321))))

  (testing "accepts keywords"
    (let [run (simulate-event! (start! fl-keywords :a 1 :b 10 :c 100) :event 1000)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 1111)))))

(deflow conditional-suspend [test]
  (if test
    (suspend! :permit :then)
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:unit SimpleConditionals
  (testing "conditional suspends in then expr"
    (clear-log!)
    (let [run (start! conditional-suspend true)]
      (testing "before event")
      (is (run-in-state? run :suspended))
      (is-log [])
      (testing "after event"
        (let [run (simulate-event! run :then)]
          (is (= (:result run) :final-value))
          (is-log [:done])))))

  (testing "but conditional does not suspend in else expr"
    (clear-log!)
    (let [run (start! conditional-suspend false)]
      (is (= (:result run) :final-value))
      (is-log [:else :done]))))

(deflow nested-conditional-suspend [level1 level2]
  (if level1
    (if level2
      (suspend! :permit :true-true)
      (log! :true-false))
    (if level2
      (log! :false-true)
      (suspend! :permit :false-false)))
  (log! :done))

(deftest ^:unit NestedConditionals
  (testing "suspend! correctly suspends inside nested then"
    (clear-log!)
    (let [run (start! nested-conditional-suspend true true)]

      (is (run-in-state? run :suspended))
      (let [run-after-event (simulate-event! run :true-true)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done]))))

  (testing "non-suspending expression in nested else returns immediately"
    (clear-log!)
    (let [run (start! nested-conditional-suspend true false)]
      (is (run-in-state? run :complete))
      (is-log [:true-false :done])))

  (testing "non-suspending expression in nested then returns immediately"
    (clear-log!)
    (let [run (start! nested-conditional-suspend false true)]
      (is (run-in-state? run :complete))
      (is-log [:false-true :done])))

  (testing "suspend! correctly suspends inside nested else"
    (clear-log!)
    (let [run (start! nested-conditional-suspend false false)]

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
      (let [run (start! conditional-with-suspending-test true)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :suspended)))

        (let [run (simulate-event! run)]
          (testing "it should return the then expression value"
            (is (= (:result run) :then-val))))))

    (testing "when test is falsey"
      (let [run (start! conditional-with-suspending-test false)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :suspended)))

        (let [run (simulate-event! run)]
          (testing "it should return the else expression value"
            (is (= (:result run) :else-val))))))))

(deflow non-suspending-let-flow [a]
  (let [b (+ 1 a)
        c (* b a)]
    (if false (suspend! :permit :foo)) ; satisfy deflow suspend requirement but do nothing
    [a b c]))

(deflow suspending-let-initial-binding-flow [arg]
  (let [suspend-value (suspend! :permit :initial-binding)
        square        (* arg arg)]
    [suspend-value square]))

(deflow suspending-let-internal-binding-flow [arg]
  (let [square        (* arg arg)
        suspend-value (suspend! :permit :internal-binding)
        cube          (* arg arg arg)]
    [square suspend-value cube]))

(deflow suspending-let-final-binding-flow [arg]
  (let [square        (* arg arg)
        cube          (* arg arg arg)
        suspend-value (suspend! :permit :final-binding)]
    [square cube suspend-value]))

(deflow suspending-let-body-flow [arg]
  (let [square (* arg arg)
        cube   (* arg arg arg)]
    [square cube (suspend! :permit :body)]))

(deftest ^:unit LetTest
  (testing "non-suspending let expressions work"
    (let [run (start! non-suspending-let-flow 2)]
      (is (= (-> run :result) [2 3 6]))))

  (testing "correctly binds a suspending initial value"
    (let [run (simulate-event! (start! suspending-let-initial-binding-flow 3) :initial-binding "event-data")]
      (is (= (:result run) ["event-data", 9]))))

  (testing "correctly binds a suspending internal value"
    (let [run (simulate-event! (start! suspending-let-internal-binding-flow 3) :internal-binding "event-data")]
      (is (= (:result run) [9, "event-data", 27]))))

  (testing "correctly binds a suspending final value"
    (let [run (simulate-event! (start! suspending-let-final-binding-flow 3) :final-binding "event-data")]
      (is (= (:result run) [9, 27, "event-data"]))))

  (testing "correctly handles body with suspending value"
    (let [run (simulate-event! (start! suspending-let-body-flow 3) :body "body-event-data")]
      (is (= (:result run) [9, 27, "body-event-data"])))))

(deflow simple-loop [n]
  (log! :before-suspend)
  (suspend! :permit :initial-wait)
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
    (if (suspend! :permit :continue-loop)
      (recur (+ a 1))
      a)))

(deftest ^:unit LoopTest
  (testing "simple loop with no suspending operations works like a normal loop"
    (clear-log!)
    (let [run (start! simple-loop 3)]
      (is (run-in-state? run :suspended))
      (is-log [:before-suspend])
      (let [run (simulate-event! run :initial-wait)]
        (is (run-in-state? run :complete))
        (is-log [:before-suspend :after-suspend :looped :looped :looped])
        (is (:result run) 3))))

  (testing "loop with suspend in body"
    (testing "single iteration"
      (clear-log!)
      (let [run (start! loop-with-suspending-body)]
        (is (run-in-state? run :suspended))
        (let [run (simulate-event! run :continue-loop false)]
          (is (run-in-state? run :complete))
          (is-log [:before-loop :inside-loop])
          (is (= (:result run) 1)))))

    (testing "multiple iterations"
      (clear-log!)
      (let [run (start! loop-with-suspending-body) ; a = 1
            run (simulate-event! run :continue-loop true) ; a + 1 = 2
            run (simulate-event! run :continue-loop true) ; a + 1 = 3
            run (simulate-event! run :continue-loop false)] ; a + 1 = 4
        (is (run-in-state? run :complete))
        (is (:result run) 4)
        (is-log [:before-loop :inside-loop :inside-loop :inside-loop])))

    (testing "throws if loop and recur bindings don't match"
      (testing "recur has more bindings"
        (is (thrown-with-msg?
              Exception #"Mismatched argument count to recur"
              (longterm.deflow/expand-flow
                `foo "" []
                '((suspend! :permit :foo)
                  (loop [a 1]
                    (recur 2 :extra)))))))
      (testing "loop has more bindings"
        (is (thrown-with-msg?
              Exception #"Mismatched argument count to recur"
              (longterm.deflow/expand-flow
                `foo "" []
                '((suspend! :permit :foo)
                  (loop [a 1 b 2]
                    ;; recur has extra argument
                    (recur 2))))))))
    (testing "throws if recur is in non-tail position"
      (testing "non suspending loop"
        (is (thrown-with-msg?
              Exception #"Can only recur from tail position"
              (longterm.deflow/expand-flow
                `foo "" []
                '((suspend! :permit :a)
                  (loop [a 1]
                    (recur 2)
                    (println "I'm in the tail pos")))))))
      (testing "suspending loop"
        (is (thrown-with-msg?
              Exception #"Can only recur from tail position"
              (longterm.deflow/expand-flow
                `foo "" []
                '((loop [a 1]
                    (suspend! :permit :a)
                    (recur 2)
                    (println "I'm in the tail pos"))))))))))

(deflow responding-flow []
  (respond! :r1)
  (suspend! :permit :s1)
  (respond! :r2)
  (respond! :r3)
  (suspend! :permit :s2)
  (respond! :r4 :r5))

(deftest ^:unit Respond
  (letfn [(response? [run x] (= (:response run) x))]
    (testing "respond! adds to an element to the current run's response"
      (let [run1 (start! responding-flow)
            run2 (simulate-event! run1 :s1)
            run3 (simulate-event! run2 :s2)]
        (testing "responds during start flow"
          (is (response? run1 [:r1])))
        (testing "Each run starts a new response, and responses accumulate during a runlet"
          (is (response? run2 [:r2 :r3])))
        (testing "respond! treats multiple arguments as separate responses")
        (is (response? run3 [:r4 :r5]))))))

(deflow datastructures []
  {:a (suspend! :permit :data) :b (suspend! :permit :data) :c [(suspend! :permit :data) {:d (suspend! :permit :data)}]})

(deftest ^:unit DataStructures
  (testing "nested data structure with multiple suspending operations"
    (let [run (reduce #(simulate-event! %1 :data %2) (start! datastructures) [1 2 3 4])]
      (is (run-in-state? run :complete))
      (is (= (:result run)
            {:a 1 :b 2 :c [3 {:d 4}]})))))

(deflow macroexpansion []
  (or (suspend! :permit :data) (and (suspend! :permit :data) (suspend! :permit :data))))

(deftest ^:unit MacroexpansionTest
  (testing "macroexpansion with suspending forms"
    (let [run (reduce #(simulate-event! %1 :data %2) (start! macroexpansion) [false true "foo"])]
      (is (run-in-state? run :complete))
      (is (= (:result run) "foo")))))

(deflow flow-with-anonymous-fn [] (map #(* % %) (suspend! :permit :list)))

(deftest ^:unit FunctionTest
  (testing "should throw an error attempting to partition a fn with suspending expressions"
    (is (thrown-with-msg?
          Exception #"Illegal attempt to suspend in function body"
          (longterm.deflow/expand-flow
            `fn-with-suspend "" []
            '((fn [] (suspend! :permit :boo)))))))

  (testing "should successfully partition when a normal fn is present in the body"
    (let [run (simulate-event! (start! flow-with-anonymous-fn) :list '(1 2 3))]
      (is (run-in-state? run :complete))
      (is (= (:result run) '(1 4 9))))))

(deftest ^:unit FlowsAsFunctions
  (testing "At top level, invoking a flow works and returns a new run"
    (is (run-in-state? (suspending-flow :foo) :suspended))))

(deflow simple-child-flow []
  (log! :child-flow-entered)
  (respond! :child-flow-response)
  (suspend!)
  (respond! :child-flow-after-continuation)
  :child-result)

(deflow parent-flow-will-block []
  (respond! :parent-before-blocking-call)
  (let [child-run (start! simple-child-flow)]
    (log! child-run)          ; we need to capture the child run to advance it
    (let [result (<! child-run)]
      (respond! :parent-after-blocking-call)
      result)))

(deftest ^:unit BlockingOperator
  (testing "Before blocking, the parent run is returned by the start operator"
    (clear-log!)
    (let [parent-run (start! parent-flow-will-block)
          child-run  (second @*log*)]
      (is (run-in-state? parent-run :suspended))
      (is (run-in-mode? parent-run :default))
      (is (-> parent-run :parent-run-id nil?))
      (is (= '(:parent-before-blocking-call) (:response parent-run)))

      (testing "The child run is created, but is not returned initially"
        (is child-run)
        (is (:id child-run))
        (is (:id parent-run))
        (is (not (= (:id child-run) (:id parent-run)))))

      (testing "After blocking, the parent run is returned in suspended state and cannot be continued"
        (let [prun    (-> parent-run :id rs/get-run)
              suspend (:suspend prun)]
          (is (= (:state prun) :suspended))
          (is (-> prun :suspend longterm.runloop/suspend-signal?))
          (is (= (-> prun :suspend :permit) (:id child-run))))
        (testing "attempting to continue without a valid permit throws"
          (is (thrown-with-msg?
                Exception #"Cannot acquire Run .* invalid permit"
                (continue! (:id parent-run))))
          (testing "but the parent run is still suspended"
            (is (= (-> parent-run :id rs/get-run :state) :suspended)))))

      (let [child-run-after-block (rs/get-run (:id child-run))]

        (testing "the child run is in block mode and keeps a record of the parent id"
          (is (= :block (:mode child-run-after-block)))
          (is (= (:parent-run-id child-run-after-block) (:id parent-run))))

        (testing "the child response includes only the response from the child run"
          (is (= '(:child-flow-response) (:response child-run-after-block))))

        (testing "the parent response includes only the response from the parent run"
          (is (= '(:parent-before-blocking-call) (:response parent-run))))

        (testing "Continuing the child run..."
          (let [completed-child (continue! (:id child-run))]
            (testing "returns a completed child run"
              (is (run-in-state? completed-child :complete))
              (is (= (:id child-run) (:id completed-child))))
            (testing "child response should contain only the child response"
              (is (= '(:child-flow-after-continuation) (:response completed-child))))

            (let [parent-after-block-release (rs/get-run (:id parent-run))]
              (testing "parent response should contain only the parent response"
                (is (= '(:parent-after-blocking-call) (:response parent-after-block-release))))

              (testing "parent should be in complete state"
                (is (run-in-state? parent-after-block-release :complete)))

              (testing "parent result should be set correctly, which in this case is the result of the blocking call"
                (is (= :child-result (:result parent-after-block-release)))))))))))