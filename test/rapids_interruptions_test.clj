(ns rapids-interruptions-test
  (:require [clojure.test :refer :all]
            [matchure.core :refer :all]
            [rapids :refer :all]
            [test-helpers :refer :all]))

(deflow interruptible-child []
  (<*))

(deflow simple-interruptible-flow []
  (attempt (<*) (handle :foo i :foo-interruption)))

(def attempt-holder (atom nil))

(deflow interruptible-flow []
  (let [attempt-val (attempt
                      (let [result (restartable (interruptible-child)
                                     (:redo [o] {:redo-value o}))]
                        (reset! attempt-holder rapids.runtime.globals/*attempts*)
                        (>* :body-called)
                        [result :uninterrupted-result])

                      (handle :foo i
                        (>* [:foo-handled i])
                        :foo-interruption)

                      (handle :bar i
                        (>* [:bar-handled i])
                        (let [interrupter-input (<*)]
                          [interrupter-input :bar-interruption]))

                      (handle :baz i
                        (restart :redo (:data i)))

                      (finally (>* :finally-called)))
        final-input (<*)]
    {:attempt-result attempt-val
     :final-input    final-input}))

(deftest ^:language InterruptionsTest
  (testing "A simple interruptible flow"
    (with-test-env
      (testing "without interruptions, block returns normally"
        (let [{initial-state :state, :as run} (start! simple-interruptible-flow)]
          (is (= :running initial-state))
          (continue! run :input "input")
          (is (= :complete (:state run)))
          (is (= "input" (:result run)))))
      (testing "with interruption, block returns the handler result"
        (let [run (start! simple-interruptible-flow)]
          (interrupt! run :foo)
          (is (= :complete (:state run)))
          (is (= :foo-interruption (:result run)))))))
  (testing "A flow with an attempt handler calling a child flow which gets interrupted while it waits for input"
    (with-test-env
      (testing "without interruptions, the attempt block should return normally"
        (let [run (start! interruptible-flow)
              _   (flush-cache!)
              run (continue! run :input :child-input)]
          (is (= :running (:state run)))
          (testing "the finally block should execute after the body"
            (is (= [:body-called :finally-called] (:output run))))
          (continue! run :input :final)
          (flush-cache!)
          (is (= :complete (:state run)))
          (is (= {:attempt-result [:child-input :uninterrupted-result]
                  :final-input    :final} (:result run)))))

      (testing "interrupting a run and handling the interruption"
        (let [run (start! interruptible-flow)
              _   (flush-cache!)
              i   (->interruption :foo)
              run (interrupt! run i)]

          (testing "the run stays in :running mode because the handler deals with the interrupt and resumes the run"
            (is (= :running (:state run)))
            (is (nil? (:interrupt run))))

          (testing "however, we see that the handler was triggered and the finally clause was executed by observing the response"
            (is (= [[:foo-handled i] :finally-called]
                  (:output run))))

          (testing "the handler return value is returned by the attempt form"
            (flush-cache!)
            (continue! run :input :continue-value)
            (is (= {:attempt-result :foo-interruption
                    :final-input    :continue-value}
                  (:result run)))))))

    (with-test-env
      (testing "interrupting a run which doesn't handle the provided interruptions throws an error"
        (let [run (start! interruptible-flow)]
          (is (throws-error-output #"Unhandled interruption" (interrupt! run (->interruption :no-handler-for-this)))))))

    (with-test-env
      (testing "testing the :bar interruption handler which uses input!"
        (let [run (start! interruptible-flow)
              _   (flush-cache!)
              i   (->interruption :bar)
              run (interrupt! run i)]

          (testing "the run goes contains an interrupt-id when the handler waits for input"
            (is (uuid? (:interrupt run))))

          (testing "attempting to continue without providing the interrupt results in an exception"
            (is (throws-error-output #"Attempt to continue interrupted run"
                  (continue! run :input :hello))))

          (testing "attempting to continue with an invalid interrupt results in an exception"
            (is (throws-error-output #"Attempt to continue interrupted run"
                  (continue! run :input :hello :interrupt :invalid))))

          (testing "Providing the interrupt value to continue allows us to continue"
            (flush-cache!)
            (continue! run
              :input :interruption-data
              :interrupt (:interrupt run))

            (is (= :running (:state run))))

          (testing "The continued input is captured within the handler"
            (continue! run :input :final)

            (is (= :complete (:state run)))
            (is (= [{:attempt-result [:interruption-data :bar-handled]
                     :final-input    :final}]))))))


    (with-test-env
      (testing "Restarting an interrupted flow"
        (let [run (start! interruptible-flow)
              _   (flush-cache!)
              i   (->interruption :baz :data :baz-data)
              run (interrupt! run i)]
          (is (= :running (:state run)))
          (continue! run :input :final)
          (is (= :complete (:state run)))
          (is (= {:attempt-result [{:redo-value :baz-data} :uninterrupted-result]
                  :final-input    :final}
                (:result run)))
          (testing "the response indicates revisiting the point where flow was interrupted"
            (is (= [:body-called :finally-called]))))))

    (with-test-env
      (testing "testing calling interrupt! with interrupt parameters instead of interrupt object"
        (let [run (start! interruptible-flow)
              _   (flush-cache!)
              run (interrupt! run :foo :message "hello" :data {:a 123})]

          (testing "the expected interruption is handled"
            (is (= [[:foo-handled (->interruption :foo :message "hello" :data {:a 123})] :finally-called]
                  (:output run)))))))))

(deftest ^:language list-interrupt-handlers-test
  (testing "list-interrupt-handlers"
    (let [run1 (start! simple-interruptible-flow)
          run2 (start! interruptible-flow)]
      (is (= '(:foo) (list-interrupt-handlers run1)))
      (is (= '(:baz :bar :foo) (list-interrupt-handlers run2))))))