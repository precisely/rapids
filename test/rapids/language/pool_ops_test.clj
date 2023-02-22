(ns rapids.language.pool-ops-test
  (:require [clojure.test :refer :all]
            [rapids :refer [current-run]]
            [rapids.implementations.in-memory-storage :refer :all]
            [rapids.language.pool-ops :refer :all]
            [rapids.objects.run :as r]
            [rapids.objects.signals :refer [suspend-signal?]]
            [rapids.runtime.run-loop :refer [defer]]
            [rapids.runtime.runlet :refer [with-run]]
            [rapids.storage.core :as s]
            [spy.core :as spy]
            [test-helpers :refer :all]))

(deftest ^:unit UnbufferedPoolTests
  (with-redefs [defer (fn [f] (f))]
    (testing "An empty unbuffered pool, when putting in"
      (with-test-env-run [p (->pool)
                          run-id (current-run :id)]
        (testing "it should suspend"
          (is (suspend-signal? (put-in! p :foo))))
        (testing "after suspending, a single entry should be in :sources."
          (let [source (-> p :sources peek)]
            (is (uuid? source))
            (testing "The entry should contain the current run id "
              (is (= source run-id)))
            (testing "The buffer should contain the value put in"
              (is (= (-> p :buffer peek) :foo)))))
        (testing "take-out! should unblock the run which was suspended by put-in! by"
          (with-continue!-stub [stub :unused]
            (with-run (s/cache-insert! (r/make-run)) ; simulate takeout happening in a different run
              (testing "returning the value put in"
                (is (= (take-out! p) :foo)))
              (testing "calling continue! once with the suspended run id, no input and the pool-id as permit"
                (is (spy/called-once-with? stub ; NB: called-once-with? requires exact argument match
                      run-id :permit (pool-id p)
                      :preserve-output true)))))))) ; i.e., no :input key provided
    (testing "An empty unbuffered pool, when taking out"
      (with-test-env-run [p (->pool)
                          run-id (current-run :id)]
        (testing "it should suspend"
          (is (suspend-signal? (take-out! p))))
        (testing "after suspending, the current run's uuid should be in :sinks."
          (is (= (pool-count p :sinks) 1))
          (let [sink-id (-> (:sinks p) peek)]
            (is (= sink-id (current-run :id)))))
        (testing "put-in! should unblock the run which was suspended by take-in! by"
          (with-continue!-stub [stub :unused]
            (testing "returning nil immediately"
              (is (= (put-in! p :foo) nil)))
            (testing "calling continue! once with the suspended run id, and the put-in value, using the pool-id as permit"
              (is (spy/called-once-with? stub
                    run-id
                    :input :foo
                    :permit (pool-id p)
                    :preserve-output true)))))))))

(deftest ^:unit BufferedPoolTests
  (with-redefs [defer (fn [f] (f))]
    (testing "An empty pool with a buffer of size 2, when putting in"
      (with-test-env-run [p (->pool 2)
                          run1 (current-run)
                          run2 (s/cache-insert! (r/make-run))
                          run3 (s/cache-insert! (r/make-run))
                          run4 (s/cache-insert! (r/make-run))
                          take-out-run (s/cache-insert! (r/make-run))]
        (testing "It should allow two unbuffered put-ins"
          (with-run run1
            (is (nil? (put-in! p 1))))
          (with-run run2
            (is (nil? (put-in! p 2)))))
        (testing "But the third and fourth put-ins should suspend"
          (with-run run3
            (is (suspend-signal? (put-in! p 3))))
          (with-run run4
            (is (suspend-signal? (put-in! p 4)))))
        (testing "The queues should be as we expect"
          (is (= (pool-count p :sources) 2))
          (is (= (pool-count p :buffer) 4))
          (is (= (pool-count p :sinks) 0)))
        (with-run take-out-run
          (testing "Taking out from a buffered pool should return buffered values first and continue first suspended runs first"
            (with-continue!-stub [stub :unused]
              (is (= (take-out! p) 1))
              (is (spy/called-once-with? stub (:id run3) :permit (pool-id p) :preserve-output true))))
          (testing "A subsequent take-out will continue the next suspended run (run4) and return the second value put-in (2)"
            (with-continue!-stub [stub :unused]
              (is (= (take-out! p) 2))
              (is (spy/called-once-with? stub (:id run4) :permit (pool-id p) :preserve-output true))))
          (testing "After the sources queue is clear, further take-outs return values without calling continue!"
            (is (= (pool-count p :sources) 0))
            (with-continue!-stub [stub nil]
              (is (= (take-out! p) 3))
              (is (= (take-out! p) 4))
              (is (spy/not-called? stub)))))))

    (testing "An empty pool with a buffer of size 2, when taking out"
      (with-test-env-run [p (->pool 2)
                          run1 (current-run)
                          run2 (s/cache-insert! (r/make-run))
                          put-in-run (s/cache-insert! (r/make-run))]
        (testing "the pool should suspend the current run"
          (with-run run1
            (is (suspend-signal? (take-out! p))))
          (with-run run2
            (is (suspend-signal? (take-out! p))))
          (testing "and the queues should be as we expect"
            (is (= (pool-count p :sources) 0))
            (is (= (pool-count p :buffer) 0))
            (is (= (pool-count p :sinks) 2))))
        (testing "Subsequent put-ins should return immediately, continuing the suspended runs..."
          (with-continue!-stub [stub :unused]
            (with-run put-in-run
              (is (nil? (put-in! p 1)))
              (is (nil? (put-in! p 2))))
            (is (= (spy/calls stub)
                  [(list (:id run1) :input 1 :permit (pool-id p) :preserve-output true)
                   (list (:id run2) :input 2 :permit (pool-id p) :preserve-output true)]))))
        (testing "Once the sinks are exhausted, a subsequent put-in should fill the buffer without calling continue!"
          (with-continue!-stub [stub :unused]
            (is (= (pool-count p :sinks) 0))
            (is (nil? (put-in! p 3)))
            (is (nil? (put-in! p 4)))
            (is (= (pool-count p :sinks) 0))
            (is (= (pool-count p :buffer) 2))
            (is (= (pool-count p :sources) 0))
            (is (zero? (spy/call-count stub)))))))))

(deftest ^:unit TakeAnyTests
  (with-redefs [defer (fn [f] (f))]
    (testing "take-any!"
      (with-test-env-run [p1 (->pool)
                          p2 (->pool)
                          p3 (->pool)
                          run1 (current-run)
                          run2 (s/cache-insert! (r/make-run))
                          run3 (s/cache-insert! (r/make-run))
                          run4 (s/cache-insert! (r/make-run))
                          take-out-run (s/cache-insert! (r/make-run))]
        (testing "It should return the default with nil index when no pool has values and default is provided"
          (with-run run1
            (is (= [nil :default-val] (take-any! [p1 p2 p3] :default-val)))))
        (testing "It should suspend when no pool has values"
          (with-run run1
            (is (suspend-signal? (take-any! [p1 p2 p3])))))
        (testing "It should continue run1 when a default is provided"
          (with-run run2
            (with-continue!-stub [stub nil]
              (put-in! p2 :foo)
              (is (spy/called-once-with? stub (:id run1)
                    :input [1 :foo]
                    :permit (pool-id p2)
                    :preserve-output true)))))))))