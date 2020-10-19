(ns longterm_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]
            [longterm.in-memory-runstore :refer [in-memory-runstore?]]))

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

(deflow listening-flow
  [val]
  (log! :before-listen)
  (listen!)
  (log! :after-listen)
  val)

(deflow event-value-flow
  "Just returns an event value"
  [context] (listen! :permit context))

(deftest ^:unit BasicFlowTests
  (testing "Start and listen"
    (clear-log!)
    (let [run (start! listening-flow :foo)]
      (is (run-in-state? run :listening))
      (is-log [:before-listen])

      (testing "send event - with no permit"
        (let [ev-result (simulate-event! run)]
          (is (run-in-state? ev-result :complete))
          (is-log [:before-listen :after-listen])

          (testing "it returns the correct value"
            (is (= (:result ev-result) :foo)))))))

  (testing "listen! event provides a value"
    (let [run (simulate-event! (start! event-value-flow :foo) :foo "foo-result")]
      (is (= (:result run) "foo-result"))))

  (testing "providing a mismatched context throws an exception"
    (is (thrown? Exception (simulate-event! (start! event-value-flow :expecting) :actual)))))

(deflow fl-nest [arg context]
  (* arg (listen! :permit context)))

(deflow nested-flow-args [a]
  (fl-nest (fl-nest (fl-nest a :first) :second) :third))

(defn a [n] (* n 10))
(deflow fl-alternating []
  (+ (listen! :permit :first-arg) (a 2) (fl-nest 100 :third)))

(deflow fl-keywords [& {:keys [a b c]}]
  (+ a b c (listen! :permit :event)))

(deftest ^:unit FunctionalExpressionTest
  (testing "nested flow arguments"
    (let [run (start! nested-flow-args 2)
          run2 (simulate-event! run :first 3)
          run3 (simulate-event! run2 :second 5)
          run4 (simulate-event! run3 :third 7)]
      (is (= (:result run4) (* 2 3 5 7)))))
  (testing "various listening and non-listening args"
    (let [run (simulate-event! (simulate-event! (start! fl-alternating) :first-arg 1) :third 3)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 321))))

  (testing "accepts keywords"
    (let [run (simulate-event! (start! fl-keywords :a 1 :b 10 :c 100) :event 1000)]
      (is (run-in-state? run :complete))
      (is (= (:result run) 1111)))))

(deflow conditional-listen [test]
  (if test
    (listen! :permit :then)
    (log! :else))
  (log! :done)
  :final-value)

(deftest ^:unit SimpleConditionals
  (testing "conditional listens in then expr"
    (clear-log!)
    (let [run (start! conditional-listen true)]
      (testing "before event")
      (is (run-in-state? run :listening))
      (is-log [])
      (testing "after event"
        (let [run (simulate-event! run :then)]
          (is (= (:result run) :final-value))
          (is-log [:done])))))

  (testing "but conditional does not listen in else expr"
    (clear-log!)
    (let [run (start! conditional-listen false)]
      (is (= (:result run) :final-value))
      (is-log [:else :done]))))

(deflow nested-conditional-listen [level1 level2]
  (if level1
    (if level2
      (listen! :permit :true-true)
      (log! :true-false))
    (if level2
      (log! :false-true)
      (listen! :permit :false-false)))
  (log! :done))

(deftest ^:unit NestedConditionals
  (testing "listen! correctly listens inside nested then"
    (clear-log!)
    (let [run (start! nested-conditional-listen true true)]

      (is (run-in-state? run :listening))
      (let [run-after-event (simulate-event! run :true-true)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done]))))

  (testing "non-listening expression in nested else returns immediately"
    (clear-log!)
    (let [run (start! nested-conditional-listen true false)]
      (is (run-in-state? run :complete))
      (is-log [:true-false :done])))

  (testing "non-listening expression in nested then returns immediately"
    (clear-log!)
    (let [run (start! nested-conditional-listen false true)]
      (is (run-in-state? run :complete))
      (is-log [:false-true :done])))

  (testing "listen! correctly listens inside nested else"
    (clear-log!)
    (let [run (start! nested-conditional-listen false false)]

      (is (run-in-state? run :listening))
      (let [run-after-event (simulate-event! run :false-false)]
        (is (run-in-state? run-after-event :complete))
        (is-log [:done])))))

(deflow conditional-with-listening-test [val]
  (if (listening-flow val)
    :then-val
    :else-val))

(deftest ^:unit ListeningTest
  (testing "if expression where the test listens, "
    (testing "when test is truthy"
      (let [run (start! conditional-with-listening-test true)]
        (testing "the expression should listen, and"
          (is (run-in-state? run :listening)))

        (let [run (simulate-event! run :test-event)]
          (testing "it should return the then expression value"
            (is (= (:result run) :then-val))))))

    (testing "when test is falsey"
      (let [run (start! conditional-with-listening-test false)]
        (testing "the expression should listen, and"
          (is (run-in-state? run :listening)))

        (let [run (simulate-event! run :test-event)]
          (testing "it should return the else expression value"
            (is (= (:result run) :else-val))))))))

(deflow non-listening-let-flow [a]
  (let [b (+ 1 a)
        c (* b a)]
    (if false (listen! :permit :foo))                       ; satisfy deflow listen requirement but do nothing
    [a b c]))

(deflow listening-let-initial-binding-flow [arg]
  (let [listen-value (listen! :permit :initial-binding)
        square (* arg arg)]
    [listen-value square]))

(deflow listening-let-internal-binding-flow [arg]
  (let [square (* arg arg)
        listen-value (listen! :permit :internal-binding)
        cube (* arg arg arg)]
    [square listen-value cube]))

(deflow listening-let-final-binding-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)
        listen-value (listen! :permit :final-binding)]
    [square cube listen-value]))

(deflow listening-let-body-flow [arg]
  (let [square (* arg arg)
        cube (* arg arg arg)]
    [square cube (listen! :permit :body)]))

(deftest ^:unit LetTest
  (testing "non-listening let expressions work"
    (let [run (start! non-listening-let-flow 2)]
      (is (= (-> run :result) [2 3 6]))))

  (testing "correctly binds a listening initial value"
    (let [run (simulate-event! (start! listening-let-initial-binding-flow 3) :initial-binding "event-data")]
      (is (= (:result run) ["event-data", 9]))))

  (testing "correctly binds a listening internal value"
    (let [run (simulate-event! (start! listening-let-internal-binding-flow 3) :internal-binding "event-data")]
      (is (= (:result run) [9, "event-data", 27]))))

  (testing "correctly binds a listening final value"
    (let [run (simulate-event! (start! listening-let-final-binding-flow 3) :final-binding "event-data")]
      (is (= (:result run) [9, 27, "event-data"]))))

  (testing "correctly handles body with listening value"
    (let [run (simulate-event! (start! listening-let-body-flow 3) :body "body-event-data")]
      (is (= (:result run) [9, 27, "body-event-data"])))))

(deflow simple-loop [n]
  (log! :before-listen)
  (listen! :permit :initial-wait)
  (log! :after-listen)
  (loop [a 1]
    (log! :looped)
    (if (< a n)
      (recur (+ a 1))
      n)))

(deflow loop-with-listening-body []
  (log! :before-loop)
  (loop [a 1]
    (log! :inside-loop)
    (if (listen! :permit :continue-loop)
      (recur (+ a 1))
      a)))

(deftest ^:unit LoopTest
  (testing "simple loop with no listening operations works like a normal loop"
    (clear-log!)
    (let [run (start! simple-loop 3)]
      (is (run-in-state? run :listening))
      (is-log [:before-listen])
      (let [run (simulate-event! run :initial-wait)]
        (is (run-in-state? run :complete))
        (is-log [:before-listen :after-listen :looped :looped :looped])
        (is (:result run) 3))))

  (testing "loop with listen in body"
    (testing "single iteration"
      (clear-log!)
      (let [run (start! loop-with-listening-body)]
        (is (run-in-state? run :listening))
        (let [run (simulate-event! run :continue-loop false)]
          (is (run-in-state? run :complete))
          (is-log [:before-loop :inside-loop])
          (is (= (:result run) 1)))))

    (testing "multiple iterations"
      (clear-log!)
      (let [run (start! loop-with-listening-body)           ; a = 1
            run (simulate-event! run :continue-loop true)   ; a + 1 = 2
            run (simulate-event! run :continue-loop true)   ; a + 1 = 3
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
                '((listen! :permit :foo)
                  (loop [a 1]
                    (recur 2 :extra)))))))
    (testing "loop has more bindings"
      (is (thrown-with-msg?
            Exception #"Mismatched argument count to recur"
            (longterm.deflow/expand-flow
              `foo "" []
              '((listen! :permit :foo)
                (loop [a 1 b 2]
                  ;; recur has extra argument
                  (recur 2))))))))
  (testing "throws if recur is in non-tail position"
    (testing "non listening loop"
      (is (thrown-with-msg?
            Exception #"Can only recur from tail position"
            (longterm.deflow/expand-flow
              `foo "" []
              '((listen! :permit :a)
                (loop [a 1]
                  (recur 2)
                  (println "I'm in the tail pos")))))))
    (testing "listening loop"
      (is (thrown-with-msg?
            Exception #"Can only recur from tail position"
            (longterm.deflow/expand-flow
              `foo "" []
              '((loop [a 1]
                  (listen! :permit :a)
                  (recur 2)
                  (println "I'm in the tail pos"))))))))) )

(deflow responding-flow []
  (respond! :r1)
  (listen! :permit :s1)
  (respond! :r2)
  (respond! :r3)
  (listen! :permit :s2)
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
  {:a (listen! :permit :data) :b (listen! :permit :data) :c [(listen! :permit :data) {:d (listen! :permit :data)}]})

(deftest ^:unit DataStructures
  (testing "nested data structure with multiple listening operations"
    (let [run (reduce #(simulate-event! %1 :data %2) (start! datastructures) [1 2 3 4])]
      (is (run-in-state? run :complete))
      (is (= (:result run)
             {:a 1 :b 2 :c [3 {:d 4}]})))))

(deflow macroexpansion []
  (or (listen! :permit :data) (and (listen! :permit :data) (listen! :permit :data))))

(deftest ^:unit MacroexpansionTest
  (testing "macroexpansion with listening forms"
    (let [run (reduce #(simulate-event! %1 :data %2) (start! macroexpansion) [false true "foo"])]
      (is (run-in-state? run :complete))
      (is (= (:result run) "foo")))))

(deflow flow-with-anonymous-fn [] (map #(* % %) (listen! :permit :list)))

(deftest ^:unit FunctionTest
  (testing "should throw an error attempting to partition a fn with listening expressions"
    (is (thrown-with-msg?
          Exception #"Illegal attempt to listen in function body"
          (longterm.deflow/expand-flow
            `fn-with-listen "" []
            '((fn [] (listen! :permit :boo)))))))

  (testing "should successfully partition when a normal fn is present in the body"
    (let [run (simulate-event! (start! flow-with-anonymous-fn) :list '(1 2 3))]
      (is (run-in-state? run :complete))
      (is (= (:result run) '(1 4 9))))))

(deftest ^:unit FlowsAsFunctions
  (testing "flows can be started like normal functions and return a run"
    (is (run-in-state? (listening-flow :foo) :listening))))