(ns rapids_test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]
            [rapids.storage.core :as storage]
            [helpers :refer [get-run get-pool throws-error-output]]
            [rapids.implementations.in-memory-storage :refer [in-memory-storage?]]
            [rapids.partitioner.core :refer [partition-flow-body]]
            [expectations.clojure.test
             :refer [defexpect expect expecting more->
                     approximately between between' functionally
                     side-effects]]
            [rapids.objects.address :as address]
            [spy.core :as spy])
  (:import (clojure.lang ExceptionInfo)))

(deftest ^:language DefaultStorageTest
  (testing "The default storage should be an InMemoryStorage"
    (is (in-memory-storage? (storage/current-storage)))))

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
  ([run & {:keys [permit data] :as keys}]
   {:pre [(run? run)]}
   (continue! (:id run) keys)))

(deflow suspending-flow
  [val]
  (log! :before-suspend)
  (<*)
  (log! :after-suspend)
  val)

(deflow event-value-flow
  "Just returns an event value"
  [permit] (<* :permit permit))

(deftest ^:language BasicFlowTests

  (testing "Start and suspend"
    (clear-log!)
    (let [run (start! suspending-flow :foo)]
      (is (run-in-state? run :running))
      (is-log [:before-suspend])

      (testing "send event - with no permit"
        (let [ev-result (simulate-event! run)]
          (is (run-in-state? ev-result :complete))
          (is-log [:before-suspend :after-suspend])

          (testing "it returns the correct value"
            (is (= (:result ev-result) :foo)))))))

  (testing "<* event provides a value"
    (let [run (simulate-event! (start! event-value-flow "foo"), :permit "foo", :data "foo-result")]
      (is (= (:result run) "foo-result"))))

  (testing "providing a mismatched context throws an exception"
    (is (thrown? Exception (simulate-event! (start! event-value-flow "expecting"), :permit "actual")))))

(deflow fl-nest [arg context]
  (* arg (<* :permit context)))

(deflow nested-flow-args [a]
  (fl-nest (fl-nest (fl-nest a "first") "second") "third"))

(defn a [n] (* n 10))
(deflow fl-alternating []
  (+ (<* :permit "first-arg") (a 2) (fl-nest 100 "third")))

(deflow fl-keywords [& {:keys [a b c]}]
  (+ a b c (<* :permit "event")))

(deftest ^:language FunctionalExpressionTest
  (testing "nested flow arguments"
    (let [run (start! nested-flow-args 2)
          run2 (simulate-event! run, :permit "first", :data 3)
          run3 (simulate-event! run2, :permit "second", :data 5)
          run4 (simulate-event! run3, :permit "third", :data 7)]
      (is (= (:result run4) (* 2 3 5 7)))))
  (testing "various suspending and non-suspending args"
    (let [run (simulate-event!
                (simulate-event! (start! fl-alternating), :permit "first-arg", :data 1),
                :permit "third", :data 3)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 321))))

  (testing "accepts keywords"
    (let [run (simulate-event! (start! fl-keywords :a 1 :b 10 :c 100),
                :permit "event", :data 1000)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 1111)))))

(deflow conditional-suspend [test]
  (if test
    (<* :permit "then")
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:language SimpleConditionals
  (testing "conditional suspends in then expr"
    (clear-log!)
    (let [run (start! conditional-suspend true)]
      (testing "before event")
      (is (run-in-state? run :running))
      (is-log [])
      (testing "after event"
        (let [run (simulate-event! run, :permit "then")]
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
      (<* :permit "true-true")
      (log! :true-false))
    (if level2
      (log! :false-true)
      (<* :permit "false-false")))
  (log! :done))

(deftest ^:language NestedConditionals
  (testing "<* correctly suspends inside nested then"
    (clear-log!)
    (let [run (start! nested-conditional-suspend true true)]

      (is (run-in-state? run :running))
      (let [run-after-event (simulate-event! run, :permit "true-true")]
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

  (testing "<* correctly suspends inside nested else"
    (clear-log!)
    (let [run (start! nested-conditional-suspend false false)]

      (is (run-in-state? run :running))
      (let [run-after-event (simulate-event! run, :permit "false-false")]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done])))))

(deflow conditional-with-suspending-test [val]
  (if (suspending-flow val)
    :then-val
    :else-val))

(deftest ^:language SuspendingTest
  (testing "if expression where the test suspends, "
    (testing "when test is truthy"
      (let [run (start! conditional-with-suspending-test true)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :running)))

        (let [run (simulate-event! run)]
          (testing "it should return the then expression value"
            (is (= (:result run) :then-val))))))

    (testing "when test is falsey"
      (let [run (start! conditional-with-suspending-test false)]
        (testing "the expression should suspend, and"
          (is (run-in-state? run :running)))

        (let [run (simulate-event! run)]
          (testing "it should return the else expression value"
            (is (= (:result run) :else-val))))))))

(deflow non-suspending-let-flow [a]
  (let [b (+ 1 a)
        c (* b a)]
    (if false (<* :permit "foo"))                           ; satisfy deflow suspend requirement but do nothing
    [a b c]))

(deflow suspending-let-initial-binding-flow [arg]
  (let [suspend-value (<* :permit "initial-binding")
        square (* arg arg)]
    [suspend-value square]))

(deflow suspending-let-internal-binding-flow [arg]
  (let [square (* arg arg)
        suspend-value (<* :permit "internal-binding")
        cube (* arg arg arg)]
    [square suspend-value cube]))

(deflow suspending-let-final-binding-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)
        suspend-value (<* :permit "final-binding")]
    [square cube suspend-value]))

(deflow suspending-let-body-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)]
    [square cube (<* :permit "body")]))

(deftest ^:language LetTest
  (testing "non-suspending let expressions work"
    (let [run (start! non-suspending-let-flow 2)]
      (is (= (-> run :result) [2 3 6]))))

  (testing "correctly binds a suspending initial value"
    (let [run (simulate-event! (start! suspending-let-initial-binding-flow 3),
                :permit "initial-binding", :data "event-data")]
      (is (= (:result run) ["event-data", 9]))))

  (testing "correctly binds a suspending internal value"
    (let [run (simulate-event! (start! suspending-let-internal-binding-flow 3),
                :permit "internal-binding", :data "event-data")]
      (is (= (:result run) [9, "event-data", 27]))))

  (testing "correctly binds a suspending final value"
    (let [run (simulate-event! (start! suspending-let-final-binding-flow 3),
                :permit "final-binding", :data "event-data")]
      (is (= (:result run) [9, 27, "event-data"]))))

  (testing "correctly handles body with suspending value"
    (let [run (simulate-event! (start! suspending-let-body-flow 3),
                :permit "body", :data "body-event-data")]
      (is (= (:result run) [9, 27, "body-event-data"])))))

(deflow non-suspending-loop [n]
  (log! :before-suspend)
  (<* :permit "initial-wait")
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
    (if (<* :permit "continue-loop")
      (recur (+ a 1))
      a)))

(deflow recur-after-suspend
  "This subtle alternative effectively tests whether partition-body handles a recur-containing expression.
  In the loop-with-suspending-body, the if expr is suspending. However in this version, the if expr is
  non-suspending."
  []
  (loop [a 1]
    (<*)
    (if (< a 2)
      (recur (+ a 1))
      :end)))

(deflow suspending-loop-with-external-params
  "Tests that non-loop bindings are preserved after suspending op."
  [n]
  (loop [a 1]
    (<*)
    (if (< a n)
      (recur (+ a 1))
      :end)))

(deftest ^:language LoopTest
  (testing "a loop with no suspending operations works like a normal loop"
    (clear-log!)
    (let [run (start! non-suspending-loop 3)]
      (is (run-in-state? run :running))
      (is-log [:before-suspend])
      (let [run (simulate-event! run, :permit "initial-wait")]
        (is (run-in-state? run :complete))
        (is-log [:before-suspend :after-suspend :looped :looped :looped])
        (is (:result run) 3))))

  (testing "loop with suspend in body"
    (testing "single iteration"
      (clear-log!)
      (let [run (start! loop-with-suspending-body)]
        (is (run-in-state? run :running))
        (let [run (simulate-event! run, :permit "continue-loop", :data false)]
          (is (run-in-state? run :complete))
          (is-log [:before-loop :inside-loop])
          (is (= (:result run) 1)))))

    (testing "multiple iterations"
      (clear-log!)
      (let [run (start! loop-with-suspending-body)          ; a = 1
            run (simulate-event! run, :permit "continue-loop", :data true) ; a + 1 = 2
            run (simulate-event! run, :permit "continue-loop", :data true) ; a + 1 = 3
            run (simulate-event! run, :permit "continue-loop", :data false)] ; a + 1 = 4
        (is (run-in-state? run :complete))
        (is (:result run) 4)
        (is-log [:before-loop :inside-loop :inside-loop :inside-loop])))

    (testing "throws if loop and recur bindings don't match"
      (testing "recur has more bindings"
        (is (thrown-with-msg?
              Exception #"Mismatched argument count to recur"
              (partition-flow-body
                {}
                (address/->address `foo)
                '([]
                  (<* :permit "foo")
                  (loop [a 1]
                    (recur 2 :extra)))))))
      (testing "loop has more bindings"
        (is (thrown-with-msg?
              Exception #"Mismatched argument count to recur"
              (partition-flow-body
                {}
                (address/->address `foo)
                '([]
                  (<* :permit "foo")
                  (loop [a 1 b 2]
                    ;; recur has extra argument
                    (recur 2))))))))
    (testing "throws if recur is in non-tail position"
      (testing "non suspending loop"
        (is (thrown-with-msg?
              Exception #"Can only recur from tail position"
              (partition-flow-body
                {}
                (address/->address `foo)
                '([]
                  (<* :permit "a")
                  (loop [a 1]
                    (recur 2)
                    (println "I'm in the tail pos")))))))
      (testing "suspending loop"
        (is (thrown-with-msg?
              Exception #"Can only recur from tail position"
              (partition-flow-body
                {}
                (address/->address `foo)
                '([] (loop [a 1]
                       (<* :permit "a")
                       (recur 2)
                       (println "I'm in the tail pos")))))))))

  (testing "recur-after-suspend"
    (let [run (start! recur-after-suspend)]
      (is (run-in-state? run :running))
      (let [run (simulate-event! run)]
        (is (run-in-state? run :running))
        (let [run (simulate-event! run)]
          (is (run-in-state? run :complete))
          (is (= (:result run) :end))))))

  (testing "suspending-loop-with-external-params"
    (let [run (start! suspending-loop-with-external-params 2)]
      (is (run-in-state? run :running))
      (let [run (simulate-event! run)]
        (is (run-in-state? run :running))
        (let [run (simulate-event! run)]
          (is (run-in-state? run :complete))
          (is (= (:result run) :end)))))))

(deflow responding-flow []
  (*> :r1)
  (<* :permit "s1")
  (*> :r2)
  (*> :r3)
  (<* :permit "s2")
  (*> :r4 :r5))

(deftest ^:language Respond
  (letfn [(response? [run x] (= (:response run) x))]
    (testing "*> adds to an element to the current run's response"
      (let [run1 (start! responding-flow)
            run2 (simulate-event! run1, :permit "s1")
            run3 (simulate-event! run2, :permit "s2")]
        (testing "responds during start flow"
          (is (response? run1 [:r1])))
        (testing "Each run starts a new response, and responses accumulate during a runlet"
          (is (response? run2 [:r2 :r3])))
        (testing "*> treats multiple arguments as separate responses")
        (is (response? run3 [:r4 :r5]))))))

(deflow datastructures []
  {:a (<* :permit "data") :b (<* :permit "data") :c [(<* :permit "data") {:d (<* :permit "data")}]})

(deftest ^:language DataStructures
  (testing "nested data structure with multiple suspending operations"
    (let [run (reduce #(simulate-event! %1, :permit "data", :data %2) (start! datastructures) [1 2 3 4])]
      (is (run-in-state? run :complete))
      (is (= (:result run)
            {:a 1 :b 2 :c [3 {:d 4}]})))))

(deflow macroexpansion []
  (or (<* :permit "data") (and (<* :permit "data") (<* :permit "data"))))

(deftest ^:language MacroexpansionTest
  (testing "macroexpansion with suspending forms"
    (let [run (reduce #(simulate-event! %1, :permit "data", :data %2) (start! macroexpansion) [false true "foo"])]
      (is (run-in-state? run :complete))
      (is (= (:result run) "foo")))))

(deflow flow-with-anonymous-fn [] (map #(* % %) (<* :permit "list")))

(deflow flow-with-closure [captured uncaptured-list]
  (let [closure #(* % captured)]
    (map closure uncaptured-list)))

(deftest ^:language FunctionTest
  (testing "should throw an error attempting to partition a fn with suspending expressions"
    (is (thrown-with-msg? Exception #"Illegal attempt to suspend in function body"
          (partition-flow-body
            {}
            (address/->address `fn-with-suspend) `([] (fn [] (listen! :permit "boo")))))))

  (testing "flow-with-closure"
    (let [run (start! flow-with-closure 2 [3 4 5])]
      (is (run? run))
      (is (= (:result run) '(6 8 10)))))

  (testing "should successfully partition when a normal fn is present in the body"
    (let [run (simulate-event! (start! flow-with-anonymous-fn), :permit "list", :data '(1 2 3))]
      (is (run-in-state? run :complete))
      (is (= (:result run) '(1 4 9))))))

(deftest ^:language FlowsAsFunctions
  (testing "Attempt to invoke flow dynamically is caught with an exception"
    (expect (more->
              ExceptionInfo type
              #"Improperly invoked flow" ex-message
              :runtime-error (-> ex-data :type))
      (suspending-flow :foo))))

(deflow simple-child-flow []
  (log! (current-run))
  (*> :child-flow-response)
  (<*)
  (*> :child-flow-after-continuation)
  :child-result)

(deflow parent-flow-will-block []
  (clear-log!)
  (log! (current-run))
  (*> :parent-before-blocking-call)
  (let [result (<<! (! simple-child-flow))]
    (*> :parent-after-blocking-call)
    result))

(deftest ^:language BlockingOperator
  (testing "Before blocking, the parent run is returned by the start operator"
    (let [returned-run (start! parent-flow-will-block)
          [parent-run, child-run] @*log*]
      (is (= (:id returned-run) (:id parent-run)))
      (is (run-in-state? returned-run :running))
      (is (-> parent-run :parent-run-id nil?))
      (is (= '(:parent-before-blocking-call) (:response returned-run)))
      (testing "The child run is created, but is not returned initially"
        (is child-run)
        (is (:id child-run))
        (is (:id parent-run))
        (is (not (= (:id child-run) (:id parent-run)))))

      (testing "After blocking, the parent run is returned, suspended with a child-run id as permit"
        (let [parent-after-block (-> parent-run :id get-run)]
          (is (= (:state parent-after-block) :running))
          (is (-> parent-after-block :suspend rapids.objects.signals/suspend-signal?))
          (is (= (-> parent-after-block :suspend :permit) (:id child-run)))

          (testing "attempting to continue without a valid permit throws"
            (expect (more->
                      ExceptionInfo type
                      #"Invalid permit. Unable to continue run." ex-message
                      :input-error (-> ex-data :type))
              (continue! (:id parent-after-block))))

          (testing "but the parent run is still suspended"
            (is (= (-> parent-after-block :id get-run :state) :running)))

          (testing "the parent response includes only the response from the parent run"
            (is (= '(:parent-before-blocking-call) (:response parent-after-block)))))

        (let [child-run-after-block (get-run (:id child-run))]

          (testing "the child run has a record of its parent id"
            (is (= (:parent-run-id child-run-after-block) (:id parent-run))))

          (testing "the child response includes only the response from the child run"
            (is (= '(:child-flow-response) (:response child-run-after-block)))))


        (testing "Continuing the child run..."
          (let [completed-child (simulate-event! child-run)]
            (testing "returns a completed child run"
              (is (run-in-state? completed-child :complete))
              (is (= (:id child-run) (:id completed-child))))
            (testing "child response should contain only the child response"
              (is (= '(:child-flow-after-continuation) (:response completed-child))))

            (let [parent-after-block-release (get-run (:id parent-run))]
              (testing "parent response should contain only the parent response"
                (is (= '(:parent-after-blocking-call) (:response parent-after-block-release))))

              (testing "parent should be in complete state"
                (is (run-in-state? parent-after-block-release :complete)))

              (testing "parent result should be set correctly, which in this case is the result of the blocking call"
                (is (= :child-result (:result parent-after-block-release)))))))))))

(deflow level3-suspends [suspend?]
  #_(println "LEVEL3: " (:id (current-run)))
  (respond! :level3-start)                                  ; this does not get captured by level1 or level2 because the redirect operator is not used
  #_(println "before level3 suspend")
  (if suspend? (listen!))
  #_(println "after level3 suspend")
  (respond! :level3-end)
  :level3-result)

(deflow level2-suspends-and-blocks [suspend-blocker?]
  #_(println "inside level2")
  (respond! :level2-start)
  (listen!)                                                 ;; up to this point is capture by level1-start
  (clear-log!)                                              ;; continue level2-run should start here
  (respond! :level2-after-suspend)
  #_(println "before level3 start")
  (let [level3 (start! level3-suspends suspend-blocker?)]
    (log! level3)                                           ;; continue level2-run ends here
    #_(println "before level3 block")
    (respond! (block! level3))
    #_(println "after level3 block"))
  (respond! :level2-end)
  :level2-result)

(deflow my-respond [a1 a2 a3] (respond! a1 a2 a3))

(deflow call-flows [forms]
  (doseq [[op a1 a2 a3] forms]
    (fcall op a1 a2 a3)))

(deflow apply-flows [forms]
  (doseq [[op a1 & rest-args] forms]
    (fapply op a1 rest-args)))

(deftest ^:language CallFlow
  (testing "fcall invokes flow objects with the supplied arguments"
    (let [run (start! call-flows [[my-respond 1 2 3] [my-respond 3 4 5]])]
      (is (= (:state run) :complete))
      (is (= (:response run [1 2 3 4 5 6])))))

  (testing "fcall invokes flow associated with flow symbols with the supplied arguments"
    (let [run (start! call-flows `[[my-respond 1 2 3] [my-respond 3 4 5]])]
      (is (= (:state run) :complete))
      (is (= (:response run [1 2 3 4 5 6]))))))

(deftest ^:language ApplyFlow
  (testing "fapply applies a flow object to the remaining args"
    (let [run (start! apply-flows [[my-respond 1 2 3] [my-respond 3 4 5]])]
      (is (= (:state run) :complete))
      (is (= (:response run [1 2 3 4 5 6])))))

  (testing "fapply applies a flow symbol to the remaining args"
    (let [run (start! apply-flows `[[my-respond 1 2 3] [my-respond 3 4 5]])]
      (is (= (:state run) :complete))
      (is (= (:response run [1 2 3 4 5 6]))))))

(deftest ^:language Destructuring
  (testing "quick and dirty tests that destructuring expressions compile without error"
    (is (flow? (var-get (eval `(deflow ~'foo [~'s] (let [[~'head & ~'rest] ~'s, ~'val (<*)] (* ~'head ~'val)))))))
    (is (flow? (var-get (eval `(deflow ~'foo [~'s] (loop [[~'head & ~'rest] ~'s] (<*) (if ~'rest (recur ~'rest))))))))))

(deflow anonymous-flow-creator [v]
  (let [f (flow [] {:received (listen!)
                    :captured v})]
    (fcall f)))

(deftest ^:language AnonymousFlowTest
  (testing "Defining a flow outside of deflow should raise an error"
    (is (throws-error-output #"(?m)Invalid context: anonymous flow may only be defined inside of deflow"
          (macroexpand `(flow [] ())))))

  (testing "In a flow which defines an anonymous flow,"
    (let [r (start! anonymous-flow-creator :bar)]
      (testing "the parent flow suspends when the internal anonymous flow suspends"
        (is (= :running (:state r)))
        (let [r (try (continue! (:id r) {:data :foo})
                     (catch ExceptionInfo e {}))
              result (:result r)]
          (is (= :complete (:state r)))
          (testing "the anonymous flow can receive continue values"
            (is (= (:received result) :foo)))
          (testing "the anonymous flow represents is a closure which captures lexical bindings"
            (is (= (:captured result) :bar))))))))

(def pool-test-atom
  (atom {:user-run nil :take-out-values []}))

(defn- pool-test-report [key f & args]                      ; test helper
  (apply swap! pool-test-atom update key f args))

(deflow start-user-interaction
  []
  (let [pool (->pool)
        user-run (start! (flow []
                           (loop [user-input (<*)
                                  counter 0]
                             (println "loop #" counter "pool = " pool)
                             (put-in! pool (str "User said '" user-input "' (" counter ")"))
                             (if (= user-input "stop")
                               (do (println "HALTING user-run")
                                   (put-in! pool :halt))
                               (recur (listen!) (inc counter))))))]
    (pool-test-report :user-run (constantly user-run))
    pool))

(deflow pool-consumer
  "Starts the user interaction flow and continuously pulls out values until :halt is received"
  []
  (let [pool (start-user-interaction)]
    (loop [result (take-out! pool)]
      (pool-test-report :take-out-values conj result)
      (when-not (= result :halt)
        (recur (take-out! pool))))))

(deftest ^:language PoolTest
  (testing "Two runs communicating via a pool"
    (let [pool-consumer-run (start! pool-consumer)
          user-run-id (-> @pool-test-atom :user-run :id)]
      (println "parent run id" (:id pool-consumer-run))
      (println "user-run-id" user-run-id)
      (testing "where the parent run starts the user interaction run"
        (is (run-in-state? pool-consumer-run :running))
        (is (run-in-state? (get-run user-run-id) :running)))
      (testing "inputs to the run which acts as the pool source are received by the pool sink"
        (println "before continue! hello")
        (continue! user-run-id {:data "hello"})
        (is (run-in-state? (get-run user-run-id) :running))

        (println "before continue! sailor")
        (continue! user-run-id {:data "sailor"})
        (is (run-in-state? (get-run user-run-id) :running))

        (println "before continue! stop")
        (continue! user-run-id {:data "stop"})
        (is (run-in-state? (get-run user-run-id) :complete))
        (println (into {} (get-run user-run-id)))
        (is (= (:received-values @pool-test-atom)
              ["User said 'hello' (0)"
               "User said 'sailor' (1)"
               "User said 'stop' (2)"]))))))
