(ns rapids-interruptions-test
  (:require [clojure.test :refer :all]
            [test_helpers :refer :all]
            [matchure.core :refer :all]
            [rapids :refer :all]))
(deflow interruptible-child []
  (<*))

(deflow interruptible-flow []
  (let [attempt-val (attempt
                      (let [result (restartable (interruptible-child)
                                     (:redo [o] {:redo-value o}))]
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
        final-listen (<*)]
    {:attempt-result attempt-val
     :final-listen   final-listen}))

(deftest ^:language InterruptionsTest
  (testing "A flow with an attempt handler calling a child flow which gets interrupted while it listens"
    (with-test-env
      (testing "without interruptions, the attempt block should return normally"
        (let [run (start! interruptible-flow)
              _ (flush-cache!)
              run (continue! run :data :child-data)]
          (is (= :running (:state run)))
          (testing "the finally block should execute after the body"
            (is (= [:body-called :finally-called] (:response run))))
          (continue! run :data :final)
          (flush-cache!)
          (is (= :complete (:state run)))
          (is (= {:attempt-result [:child-data :uninterrupted-result]
                  :final-listen   :final} (:result run)))))

      (testing "interrupting a run and handling the interruption"
        (let [run (start! interruptible-flow)
              _ (flush-cache!)
              i (->interruption :foo)
              run (interrupt! run i)]

          (testing "the run stays in :running mode because the handler deals with the interrupt and resumes the run"
            (is (= :running (:state run)))
            (is (nil? (:interrupt run))))

          (testing "however, we see that the handler was triggered and the finally clause was executed by observing the response"
            (is (= [[:foo-handled i] :finally-called]
                  (:response run))))

          (testing "the handler return value is returned by the attempt form"
            (flush-cache!)
            (continue! run :data :continue-value)
            (is (= {:attempt-result :foo-interruption
                    :final-listen   :continue-value}
                  (:result run)))))))

    (with-test-env
      (testing "interrupting a run which doesn't handle the provided interruptions throws an error"
        (let [run (start! interruptible-flow)]
          (is (throws-error-output #"Unhandled interruption" (interrupt! run (->interruption :no-handler-for-this)))))))

    (with-test-env
      (testing "testing the :bar interruption handler which uses listen!"
        (let [run (start! interruptible-flow)
              _ (flush-cache!)
              i (->interruption :bar)
              run (interrupt! run i)]

          (testing "the run goes into :interrupted mode as the handler waits for input"
            (is (uuid? (:interrupt run))))

          (testing "attempting to continue without providing the interrupt results in an exception"
            (is (throws-error-output #"Attempt to continue interrupted run"
                  (continue! run :data :hello))))

          (testing "attempting to continue with an invalid interrupt results in an exception"
            (is (throws-error-output #"Attempt to continue interrupted run"
                  (continue! run :data :hello :interrupt :invalid))))

          (testing "Providing the interrupt value to continue allows us to continue"
            (flush-cache!)
            (continue! run
              :data :interruption-data
              :interrupt (:interrupt run))

            (is (= :running (:state run))))

          (testing "The continued data is captured within the handler"
            (continue! run :data :final)

            (is (= :complete (:state run)))
            (is (= [{:attempt-result [:interruption-data :bar-handled]
                     :final-listen   :final}]))))))


    (with-test-env
      (testing "Restarting an interrupted flow"
        (let [run (start! interruptible-flow)
              _ (flush-cache!)
              i (->interruption :baz :data :baz-data)
              run (interrupt! run i)]
          (is (= :running (:state run)))
          (continue! run :data :final)
          (is (= :complete (:state run)))
          (is (= {:attempt-result [{:redo-value :baz-data} :uninterrupted-result]
                  :final-listen   :final}
                (:result run)))
          (testing "the response indicates revisiting the point where flow was interrupted"
            (is (= [:body-called :finally-called]))))))

    (with-test-env
      (testing "testing calling interrupt! with interrupt parameters instead of interrupt object"
        (let [run (start! interruptible-flow)
              _ (flush-cache!)
              run (interrupt! run :foo :message "hello" :data {:a 123})]

          (testing "the expected interruption is handled"
            (println (:response run))
            (is (= [[:foo-handled (->interruption :foo :message "hello" :data {:a 123})] :finally-called]
                  (:response run)))))))))