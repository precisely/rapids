(ns rapids_test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]
            [rapids.storage.core :as storage :refer [cache-proxy?]]
            [test_helpers :refer [flush-cache! proxy-field with-test-env-run get-run get-pool throws-error-output run-in-state?]]
            [rapids.implementations.in-memory-storage :refer [in-memory-storage?]]
            [rapids.partitioner.core :refer [partition-flow-body]]
            [rapids.support.debug :refer :all]
            [test_helpers :refer :all]
            [expectations.clojure.test
             :refer [defexpect expect expecting more->
                     approximately between between' functionally
                     side-effects]]
            [rapids.objects.address :as address])
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
  (with-test-env
    (testing "Start and suspend"
      (clear-log!)
      (let [run (start! suspending-flow :foo)]
        (is (run-in-state? run :running))
        (is-log [:before-suspend])

        (testing "Object returned by start! is a CacheProxy"
          (is (storage/cache-proxy? run)))

        (testing "CacheProxy returned by start provides access to full object"
          (binding [rapids.storage.globals/*cache* nil]
            (assert (not (storage/cache-exists?)))
            (is (map? (.rawData run)))
            (is (= :running (:state (.rawData run))))
            (is (= :running (:state run)))))

        (testing "send event - with no permit"
          (flush-cache!)
          (let [ev-result (continue! run)]
            (is (run-in-state? ev-result :complete))
            (is-log [:before-suspend :after-suspend])

            (testing "it returns the correct value"
              (is (= (proxy-field ev-result :result) :foo)))))))

    (testing "<* event provides a value"
      (flush-cache!)
      (let [run (continue! (start! event-value-flow "foo"), :permit "foo", :data "foo-result")]
        (is (= (proxy-field run :result) "foo-result"))))

    (testing "providing a mismatched context throws an exception"
      (is (thrown? Exception (continue! (start! event-value-flow "expecting"), :permit "actual"))))))

(deftest ^:language CacheProxyTopLevelTests
  (let [run (start! suspending-flow :foo)]
    (testing "Invoking start! outside of cached connection should produce a CacheProxy with accessible raw data"
      (assert (not (storage/cache-exists?)))
      (is (map? (.rawData run))))
    (testing "Invoking continue! outside of cached connection should produce a CacheProxy with accessible raw data"
      (let [continued-run (continue! run :data :bar)]
        (assert (not (storage/cache-exists?)))
        (is (map? (.rawData continued-run)))))))

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
  (with-test-env
    (testing "nested flow arguments"
      (let [run (start! nested-flow-args 2)
            run2 (continue! run, :permit "first", :data 3)
            run3 (continue! run2, :permit "second", :data 5)
            run4 (continue! run3, :permit "third", :data 7)]
        (is (= (proxy-field run4 :result) (* 2 3 5 7)))))

    (testing "various suspending and non-suspending args"
      (flush-cache!)

      (let [run (continue!
                  (continue! (start! fl-alternating), :permit "first-arg", :data 1),
                  :permit "third", :data 3)]
        (is (run-in-state? run :complete))
        (is (= (proxy-field run :result) 321))))

    (testing "accepts keywords"
      (let [run (continue! (start! fl-keywords :a 1 :b 10 :c 100),
                  :permit "event", :data 1000)]
        (is (run-in-state? run :complete))
        (is (= (proxy-field run :result) 1111))))))

(deflow java-static-method-test [x]
  (* (Integer/parseInt (<*)) x))

(deftest ^:language JavaStaticTest
  (testing "It can use Java static methods"
    (is (= 990 (:result (continue! (start! java-static-method-test 10) :data "99"))))))

(deflow conditional-suspend [test]
  (if test
    (<* :permit "then")
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:language SimpleConditionals
  (with-test-env
    (testing "conditional suspends in then expr"
      (clear-log!)
      (let [run (start! conditional-suspend true)]
        (testing "before event")
        (is (run-in-state? run :running))
        (is-log [])
        (testing "after event"
          (let [run (continue! run, :permit "then")]
            (is (= (proxy-field run :result) :final-value))
            (is-log [:done])))))

    (testing "but conditional does not suspend in else expr"
      (clear-log!)
      (let [run (start! conditional-suspend false)]
        (is (= (proxy-field run :result) :final-value))
        (is-log [:else :done])))))

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
  (with-test-env

    (testing "<* correctly suspends inside nested then"
      (clear-log!)
      (let [run (start! nested-conditional-suspend true true)]

        (is (run-in-state? run :running))
        (let [run-after-event (continue! run, :permit "true-true")]
          (is (run-in-state? run-after-event :complete))
          (is-log [:done]))))

    (testing "non-suspending expression in nested else returns immediately"
      (flush-cache!)

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
        (let [run-after-event (continue! run, :permit "false-false")]
          (is (run-in-state? run-after-event :complete))
          (is-log [:done]))))))

(deflow conditional-with-suspending-test [val]
  (if (suspending-flow val)
    :then-val
    :else-val))

(deftest ^:language SuspendingTest
  (ensure-cached-connection
    (testing "if expression where the test suspends, "
      (testing "when test is truthy"
        (let [run (start! conditional-with-suspending-test true)]
          (testing "the expression should suspend, and"
            (is (run-in-state? run :running)))

          (flush-cache!)

          (let [run (continue! run)]
            (testing "it should return the then expression value"
              (is (= (proxy-field run :result) :then-val))))))

      (testing "when test is falsey"
        (let [run (start! conditional-with-suspending-test false)]
          (testing "the expression should suspend, and"
            (is (run-in-state? run :running)))

          (let [run (continue! run)]
            (testing "it should return the else expression value"
              (is (= (proxy-field run :result) :else-val)))))))))

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
  (storage/ensure-cached-connection
    (testing "non-suspending let expressions work"
      (let [run (start! non-suspending-let-flow 2)]
        (is (= (proxy-field run :result) [2 3 6]))))

    (testing "correctly binds a suspending initial value"
      (let [run (continue! (start! suspending-let-initial-binding-flow 3),
                  :permit "initial-binding", :data "event-data")]
        (is (= (proxy-field run :result) ["event-data", 9]))))

    (testing "correctly binds a suspending internal value"
      (let [run (continue! (start! suspending-let-internal-binding-flow 3),
                  :permit "internal-binding", :data "event-data")]
        (is (= (proxy-field run :result) [9, "event-data", 27]))))

    (testing "correctly binds a suspending final value"
      (let [run (continue! (start! suspending-let-final-binding-flow 3),
                  :permit "final-binding", :data "event-data")]
        (is (= (proxy-field run :result) [9, 27, "event-data"]))))

    (testing "correctly handles body with suspending value"
      (let [run (continue! (start! suspending-let-body-flow 3),
                  :permit "body", :data "body-event-data")]
        (is (= (proxy-field run :result) [9, 27, "body-event-data"]))))))

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
  (storage/ensure-cached-connection
    (testing "a loop with no suspending operations works like a normal loop"
      (clear-log!)
      (let [run (start! non-suspending-loop 3)]
        (is (run-in-state? run :running))
        (is-log [:before-suspend])
        (let [run (continue! run, :permit "initial-wait")]
          (is (run-in-state? run :complete))
          (is-log [:before-suspend :after-suspend :looped :looped :looped])
          (is (proxy-field run :result) 3))))

    (testing "loop with suspend in body"
      (testing "single iteration"
        (clear-log!)
        (let [run (start! loop-with-suspending-body)]
          (is (run-in-state? run :running))
          (let [run (continue! run, :permit "continue-loop", :data false)]
            (is (run-in-state? run :complete))
            (is-log [:before-loop :inside-loop])
            (is (= (proxy-field run :result) 1)))))

      (testing "multiple iterations"
        (clear-log!)
        (let [run (start! loop-with-suspending-body)        ; a = 1
              run (continue! run, :permit "continue-loop", :data true) ; a + 1 = 2
              run (continue! run, :permit "continue-loop", :data true) ; a + 1 = 3
              run (continue! run, :permit "continue-loop", :data false)] ; a + 1 = 4
          (is (run-in-state? run :complete))
          (is (proxy-field run :result) 4)
          (is-log [:before-loop :inside-loop :inside-loop :inside-loop])))

      (testing "throws if loop and recur bindings don't match"
        (testing "recur has more bindings"
          (is (thrown-with-msg?
                Exception #"Mismatched argument count to recur"
                (partition-flow-body
                  {}
                  (address/->address `foo)
                  '([]
                    (rapids/<* :permit "foo")
                    (loop [a 1]
                      (recur 2 :extra)))))))
        (testing "loop has more bindings"
          (is (thrown-with-msg?
                Exception #"Mismatched argument count to recur"
                (partition-flow-body
                  {}
                  (address/->address `foo)
                  '([]
                    (rapids/<* :permit "foo")
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
                    (rapids/<* :permit "a")
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
                         (rapids/<* :permit "a")
                         (recur 2)
                         (println "I'm in the tail pos")))))))))

    (testing "recur-after-suspend"
      (let [run (start! recur-after-suspend)]
        (is (run-in-state? run :running))
        (let [run (continue! run)]
          (is (run-in-state? run :running))
          (let [run (continue! run)]
            (is (run-in-state? run :complete))
            (is (= (proxy-field run :result) :end))))))

    (testing "suspending-loop-with-external-params"
      (let [run (start! suspending-loop-with-external-params 2)]
        (is (run-in-state? run :running))
        (let [run (continue! run)]
          (is (run-in-state? run :running))
          (let [run (continue! run)]
            (is (run-in-state? run :complete))
            (is (= (proxy-field run :result) :end))))))))

(deflow responding-flow []
  (>* :r1)
  (<* :permit "s1")
  (>* :r2)
  (>* :r3)
  (<* :permit "s2")
  (>* :r4 :r5))

(deftest ^:language Respond
  (storage/ensure-cached-connection
    (testing ">* adds to an element to the current run's response"
      (let [run (start! responding-flow)]
        (testing "responds during start flow"
          (is (= (proxy-field run :response) [:r1])))
        (continue! run, :permit "s1")
        (testing "Each run starts a new response, and responses accumulate during a runlet"
          (is (= (proxy-field run :response) [:r2 :r3])))
        (continue! run, :permit "s2")
        (testing ">* treats multiple arguments as separate responses")
        (is (= (proxy-field run :response) [:r4 :r5]))))))

(deflow datastructures []
  {:a (<* :permit "data") :b (<* :permit "data") :c [(<* :permit "data") {:d (<* :permit "data")}]})

(deftest ^:language DataStructures
  (storage/ensure-cached-connection
    (testing "nested data structure with multiple suspending operations"
      (let [run (reduce #(continue! %1, :permit "data", :data %2) (start! datastructures) [1 2 3 4])]
        (is (run-in-state? run :complete))
        (is (= (proxy-field run :result)
              {:a 1 :b 2 :c [3 {:d 4}]}))))))

(deflow macroexpansion []
  (or (<* :permit "data") (and (<* :permit "data") (<* :permit "data"))))

(deftest ^:language MacroexpansionTest
  (storage/ensure-cached-connection
    (testing "macroexpansion with suspending forms"
      (let [run (reduce #(continue! %1, :permit "data", :data %2) (start! macroexpansion) [false true "foo"])]
        (is (run-in-state? run :complete))
        (is (= (proxy-field run :result) "foo"))))))

(deflow flow-with-anonymous-fn [] (map #(* % %) (<* :permit "list")))

(deflow flow-with-closure [captured uncaptured-list]
  (let [closure #(* % captured)]
    (map closure uncaptured-list)))

(deftest ^:language FunctionTest
  (storage/ensure-cached-connection
    (testing "should throw an error attempting to partition a fn with suspending expressions"
      (is (thrown-with-msg? Exception #"Illegal attempt to suspend in function body"
            (partition-flow-body
              {}
              (address/->address `fn-with-suspend) `([] (fn [] (listen! :permit "boo")))))))

    (testing "flow-with-closure"
      (let [run (start! flow-with-closure 2 [3 4 5])]
        (is (run? run))
        (is (= (proxy-field run :result) '(6 8 10)))))

    (testing "should successfully partition when a normal fn is present in the body"
      (let [run (continue! (start! flow-with-anonymous-fn), :permit "list", :data '(1 2 3))]
        (is (run-in-state? run :complete))
        (is (= (proxy-field run :result) '(1 4 9)))))))

(deftest ^:language FlowsAsFunctions
  (storage/ensure-cached-connection
    (testing "Attempt to invoke flow dynamically is caught with an exception"
      (expect (more->
                ExceptionInfo type
                #"Improperly invoked flow" ex-message
                :runtime-error (-> ex-data :type))
        (suspending-flow :foo)))))

(deflow simple-child-flow []
  (log! (current-run))
  (>* :child-flow-response)
  (<*)
  (>* :child-flow-after-suspending)
  :child-result)

(deflow parent-flow-will-block []
  (clear-log!)
  (log! (current-run))
  (>* :parent-before-blocking-call)
  (let [result (<<! (! simple-child-flow))]
    (>* :parent-after-blocking-call)
    result))

(deftest ^:language BlockingOperator
  (with-test-env
    (testing "Before blocking, the parent run is returned by the start operator"
      (let [returned-run (start! parent-flow-will-block)
            [parent-run, child-run] @*log*]
        (is (= (:id returned-run) (:id parent-run)))
        (is (run-in-state? returned-run :running))
        (is (-> parent-run :parent-run-id nil?))
        (is (= '(:parent-before-blocking-call) (proxy-field returned-run :response)))
        (testing "The child run is created, but is not returned initially"
          (is child-run)
          (is (:id child-run))
          (is (:id parent-run))
          (is (not (= (:id child-run) (:id parent-run)))))

        (testing "After blocking, the parent run is returned, suspended with a child-run id as permit"
          (flush-cache!)
          (let [parent-after-block (get-run (:id parent-run))]
            (is (= (:state parent-after-block) :running))
            (is (-> parent-after-block :suspend rapids.objects.signals/suspend-signal?))

            (is (= (-> parent-after-block :suspend :permit) (:id child-run)))

            (testing "attempting to continue without a valid permit throws"
              (flush-cache!)
              (expect (more->
                        ExceptionInfo type
                        #"Invalid permit. Unable to continue run." ex-message
                        :input-error (-> ex-data :type))
                (continue! (:id parent-after-block))))

            (testing "but the parent run is still suspended"
              (is (= (-> parent-after-block :id get-run :state) :running)))

            (testing "the parent response includes only the response from the parent run"
              (is (= '(:parent-before-blocking-call) (:response parent-after-block)))))

          (flush-cache!)

          (let [child-run-after-block (get-run (:id child-run))]

            (testing "the child run has a record of its parent id"
              (is (= (:parent-run-id child-run-after-block) (:id parent-run))))

            (testing "the child response includes only the response from the child run"
              (is (= '(:child-flow-response) (:response child-run-after-block)))))

          (flush-cache!)

          (flush)
          (testing "Continuing the child run..."
            (let [completed-child (continue! child-run)]
              (testing "returns a completed child run"
                (is (run-in-state? completed-child :complete))
                (is (= (:id child-run) (:id completed-child))))
              (testing "child response should contain only the child response"
                (is (= '(:child-flow-after-suspending) (:response completed-child))))

              (flush-cache!)

              (let [parent-after-block-release (get-run (:id parent-run))]
                (testing "parent response should contain only the parent response"
                  (is (= '(:parent-after-blocking-call) (:response parent-after-block-release))))

                (testing "parent should be in complete state"
                  (is (run-in-state? parent-after-block-release :complete)))

                (testing "parent result should be set correctly, which in this case is the result of the blocking call"
                  (is (= :child-result (:result parent-after-block-release))))))))))))

(deflow level3-suspends [suspend?]
  (respond! :level3-start)                                  ; this does not get captured by level1 or level2 because the redirect operator is not used
  (if suspend? (listen!))
  (respond! :level3-end)
  :level3-result)

(deflow level2-suspends-and-blocks [suspend-blocker?]
  (respond! :level2-start)
  (listen!)                                                 ;; up to this point is capture by level1-start
  (clear-log!)                                              ;; continue level2-run should start here
  (respond! :level2-after-suspend)
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
  (storage/ensure-cached-connection
    (testing "fcall invokes flow objects with the supplied arguments"
      (let [run (start! call-flows [[my-respond 1 2 3] [my-respond 4 5 6]])]
        (is (= (proxy-field run :state) :complete))
        (is (= (:response run) [1 2 3 4 5 6]))))

    (testing "fcall invokes flow associated with flow symbols with the supplied arguments"
      (flush-cache!)
      (let [run (start! call-flows `[[my-respond 1 2 3] [my-respond 4 5 6]])]
        (is (= (proxy-field run :state) :complete))
        (is (= (:response run) [1 2 3 4 5 6]))))))

(deftest ^:language ApplyFlow
  (storage/ensure-cached-connection
    (testing "fapply applies a flow object to the remaining args"
      (let [run (start! apply-flows [[my-respond 1 2 3] [my-respond 4 5 6]])]
        (is (= (proxy-field run :state) :complete))
        (is (= (:response run) [1 2 3 4 5 6]))))

    (testing "fapply applies a flow symbol to the remaining args"
      (flush-cache!)
      (let [run (start! apply-flows `[[my-respond 1 2 3] [my-respond 4 5 6]])]
        (is (= (proxy-field run :state) :complete))
        (is (= (:response run) [1 2 3 4 5 6]))))))

(deftest ^:language Destructuring
  (testing "quick and dirty tests that destructuring expressions compile without error"
    (is (flow? (var-get (eval `(deflow ~'foo [~'s] (let [[~'head & ~'rest] ~'s, ~'val (<*)] (* ~'head ~'val)))))))
    (is (flow? (var-get (eval `(deflow ~'foo [~'s] (loop [[~'head & ~'rest] ~'s] (<*) (if ~'rest (recur ~'rest))))))))))

(deflow anonymous-flow-creator [v]
  (let [f (flow [] {:received (listen!)
                    :captured v})]
    (fcall f)))

(deftest ^:language AnonymousFlowTest
  (storage/ensure-cached-connection
    (testing "Defining a flow outside of deflow should raise an error"
      (is (throws-error-output #"(?m)Invalid context: anonymous flow may only be defined inside of deflow"
            (macroexpand `(flow [] ())))))

    (testing "In a flow which defines an anonymous flow,"
      (let [r (start! anonymous-flow-creator :bar)]
        (testing "the parent flow suspends when the internal anonymous flow suspends"
          (is (= :running (proxy-field r :state)))
          (flush-cache!)
          (let [r (try (continue! (:id r) :data :foo)
                       (catch ExceptionInfo e {}))
                result (proxy-field r :result)]
            (is (= :complete (proxy-field r :state)))
            (testing "the anonymous flow can receive continue values"
              (is (= (:received result) :foo)))
            (testing "the anonymous flow represents is a closure which captures lexical bindings"
              (is (= (:captured result) :bar)))))))))

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
                             (put-in! pool (str "User said '" user-input "' (" counter ")"))
                             (if (= user-input "stop")
                               (put-in! pool :halt)
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
  (storage/ensure-cached-connection
    (testing "Two runs communicating via a pool"
      (let [pool-consumer-run (start! pool-consumer)
            user-run-id (-> @pool-test-atom :user-run :id)]
        (flush-cache!)

        (testing "where the parent run starts the user interaction run"
          (is (= (-> pool-consumer-run :id get-run :state) :running))
          (is (= (-> user-run-id get-run :state) :running)))
        (testing "inputs to the run which acts as the pool source are received by the pool sink"
          (continue! user-run-id :data "hello")
          (is (= (-> user-run-id get-run :state) :running))

          (flush-cache!)
          (continue! user-run-id :data "sailor")
          (is (= (-> user-run-id get-run :state) :running))

          (flush-cache!)
          (continue! user-run-id :data "stop")

          (flush-cache!)
          (is (= (-> user-run-id get-run :state) :complete))
          (is (= ["User said 'hello' (0)"
                  "User said 'sailor' (1)"
                  "User said 'stop' (2)"
                  :halt]
                (:take-out-values @pool-test-atom))))))))

(deflow call-cc-fn-test [t]
  (case t
    :short-circuit (+ 1 (callcc (flow [cc]
                                  (+ 2
                                    (fcall cc 3)
                                    (assert false "This code never executes")))))
    :recurrence (let [retval (callcc)]
                  (>* (if (closure? retval)
                        "callcc returns a closure"
                        (str "callcc returns a value: " retval)))
                  (if (closure? retval)
                    (fcall retval 123)))))

(deftest ^:language CallWithCurrentContinuationTest
  (testing (str "Short circuiting prevents execution of the form after invokation of the current continuation, "
             " and provides the given value at the point of callcc")
    ;; this test shows that the assert expr is never evaluated:
    ;; (+ 2 (fcall cc 3) (assert false "This code never executes"))
    (is (= 4 (:result (start! call-cc-fn-test :short-circuit)))))
  (testing "Recurrence: Calling callcc with no args returns the current continuation, and calling it later returns control to the point where it was created"
    (is (= ["callcc returns a closure" "callcc returns a value: 123"]
          (:response (start! call-cc-fn-test :recurrence))))))

(def ^:dynamic *test-binding*)
(declare dynamic-binding-test2)

(deflow dynamic-binding-child-flow
  ([pos] (dynamic-binding-child-flow pos false))
  ([pos set-binding?]
   (>* {:child [pos *test-binding*]})
   (when set-binding?
     (set! *test-binding* :set-value)
     (>* {:child-after-set [pos *test-binding*]}))))

(deflow dynamic-binding-parent-flow [set-inner-value?]
  (binding [*test-binding* :outer-value]
    (>* {:parent [:p1 *test-binding*]})
    (dynamic-binding-child-flow :p1)
    (<*)
    ;;
    ;; :p2
    ;;
    (>* {:parent [:p2 *test-binding*]})
    (dynamic-binding-child-flow :p2)
    (<*)

    ;;
    ;; :p3
    ;;

    (binding [*test-binding* :inner-value]
      (>* {:parent [:p3 *test-binding*]})

      ;;
      ;; IF set-inner-value? is true, the child flow will set *test-binding* to :set-value
      ;;
      (dynamic-binding-child-flow :p3 set-inner-value?)
      (<*)

      ;;
      ;; :p4
      ;;
      (>* {:parent [:p4-inner *test-binding*]})
      (dynamic-binding-child-flow :p4-inner))


    ;;
    ;; binding is undone
    ;;
    (>* {:parent [:p4-outer *test-binding*]})
    (dynamic-binding-child-flow :p4-outer)
    (<*)

    ;;
    ;; :p5
    ;;
    (>* {:parent [:p5 *test-binding*]})
    (dynamic-binding-child-flow :p5)))

(deftest ^:language DynamicBindingTest
  (testing "Using the binding form to generate dynamic bindings"
    (with-test-env
      (testing ""
        (let [run (start! dynamic-binding-parent-flow false)]
          (testing "Binding works normally in the first partition"
            (is (= (:response run) [{:parent [:p1 :outer-value]}
                                    {:child [:p1 :outer-value]}])))

          ;; :p2
          (flush-cache!)
          (continue! run)
          (testing "The binding holds in a subsequent partition"
            (is (= (:response run) [{:parent [:p2 :outer-value]}
                                    {:child [:p2 :outer-value]}])))

          ;; :p3
          (flush-cache!)
          (continue! run)
          (testing "The binding can be overridden"
            (is (= (:response run) [{:parent [:p3 :inner-value]}
                                    {:child [:p3 :inner-value]}])))

          ;; :p4
          (flush-cache!)
          (continue! run)
          (testing "The overriding binding holds in a subsequent partition, and can be released"
            (is (= (:response run) [{:parent [:p4-inner :inner-value]}
                                    {:child [:p4-inner :inner-value]}
                                    {:parent [:p4-outer :outer-value]}
                                    {:child [:p4-outer :outer-value]}])))

          ;; :p5
          (flush-cache!)
          (continue! run)
          (testing "Once released the binding remains at its original value in subsequent partitions"
            (is (= (:response run) [{:parent [:p5 :outer-value]}
                                    {:child [:p5 :outer-value]}]))))))))

(deftest ^:language DynamicSetTest
  (testing "Using set! to directly set a value"
    (with-test-env
      (testing ""
        (let [run (start! dynamic-binding-parent-flow true)]
          (testing "Binding works normally in the first partition"
            (is (= (:response run) [{:parent [:p1 :outer-value]}
                                    {:child [:p1 :outer-value]}])))

          ;; :p2
          (flush-cache!)
          (continue! run)
          (testing "The binding holds in a subsequent partition"
            (is (= (:response run) [{:parent [:p2 :outer-value]}
                                    {:child [:p2 :outer-value]}])))

          ;; :p3
          (flush-cache!)
          (continue! run)
          (testing "The inner binding is overriden with set!"
            (is (= (:response run) [{:parent [:p3 :inner-value]}
                                    {:child [:p3 :inner-value]}
                                    {:child-after-set [:p3 :set-value]}])))

          ;; :p4
          (flush-cache!)
          (continue! run)
          (testing "The set! binding holds in a subsequent partition, and is lost after the binding goes out of scope"
            (is (= (:response run) [{:parent [:p4-inner :set-value]} ; <<== these lines show set! has carried beyond the initial partition
                                    {:child [:p4-inner :set-value]} ; <<==
                                    {:parent [:p4-outer :outer-value]}
                                    {:child [:p4-outer :outer-value]}])))

          ;; :p5
          (flush-cache!)
          (continue! run)
          (testing "Once released the binding remains at its original value in subsequent partitions"
            (is (= (:response run) [{:parent [:p5 :outer-value]}
                                    {:child [:p5 :outer-value]}]))))))))

(def ^:dynamic *cc-dynamic*)
(def ^:dynamic *cc*)
(deflow callcc-with-dynamics-child []
  (binding [*cc-dynamic* :inner]
    ;; second response after continue!
    (>* {:child {:*cc-dynamic* *cc-dynamic*}})

    (fcall *cc* :interruption)

    (>* :this-does-not-execute)))

(deflow callcc-with-dynamics
  "Tests the interaction of callcc with dynamic bindings. With a toy interruption
  handling example."
  []
  (binding [*cc-dynamic* :outer
            *cc* (callcc)]
    (>* {:first-partition {:*cc-dynamic* *cc-dynamic*}})
    (<*)                                                    ; force a partition
    ;; AFTER continue!
    (if (closure? *cc*)
      ;; first response:
      (do (>* {:then-branch {:*cc* :closure, :*cc-dynamic* *cc-dynamic*}})
          (callcc-with-dynamics-child))

      (>* {:else-branch {:*cc* *cc*, :*cc-dynamic* *cc-dynamic*}}))))

(deftest ^:language CallCCWithDynamics
  (with-test-env
    (testing ""
      (let [run (start! callcc-with-dynamics)]
        (testing "Sanity test that *cc-dynamic* is bound properly initially"
          (is (= (:response run) [{:first-partition {:*cc-dynamic* :outer}}])))

        ;;
        ;; FIRST TRIP THROUGH
        ;;
        (flush-cache!)
        (continue! run)
        (let [response (:response run)]
          (testing "The current continuation can be captured in a trans-partition dynamic"
            (is (= (first response) {:then-branch {:*cc* :closure, :*cc-dynamic* :outer}})))

          (testing "Sanity checking that an inner dynamic binding is applied before calling the current continuation"
            (is (= (second response) {:child {:*cc-dynamic* :inner}})))

          (testing "The current continuation does indeed interrupt the child flow"
            (is (not-any? #(= :this-does-not-execute %) response)))

          (testing "The current continuation restores state at the parent flow WITH the original dynamic binding"
            (is (= (nth response 2) {:first-partition {:*cc-dynamic* :outer}})))

          (testing "Execution halts as expected at the end of the first parent partition"
            (is (= (count response) 3))
            (is (= (:state run) :running)))

          ;;
          ;; SECOND TRIP THROUGH
          ;;
          (flush-cache!)
          (continue! run)

          (testing "The second time through, the continuation call returns the value provided inside the child flow"
            (is (= [{:else-branch {:*cc* :interruption, :*cc-dynamic* :outer}}]
                  (:response run)))))))))

