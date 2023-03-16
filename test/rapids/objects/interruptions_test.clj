(ns rapids.objects.interruptions-test
  (:require [clojure.test :refer :all]
            [rapids.objects.interruptions :refer :all])
  (:import (rapids.objects.interruptions Interruption InterruptionHandler Restart Attempt)))

;; Testing Interruption record and ->interruption function
(deftest test-interruption
  (let [interruption (->interruption :foo {:bar 1})]
    (is (instance? Interruption interruption))
    (is (= (:name interruption) :foo))
    (is (= (:data interruption) {:bar 1}))))

;; Testing InterruptionHandler record
(deftest test-interruption-handler
  (let [handler (->InterruptionHandler :foo :bar {:foo true})]
    (is (instance? InterruptionHandler handler))
    (is (= :foo (:name handler)))
    (is (= :bar (:closure handler)))
    (is (= {:foo true} (:metadata handler)))))

;; Testing Restart record
(deftest test-restart
  (let [restart (->Restart :foo :bar {})]
    (is (instance? Restart restart))
    (is (= (:name restart) :foo))
    (is (= (:closure restart) :bar))
    (is (= (:metadata restart) {}))))

;; Testing Attempt record
(deftest test-attempt
  (let [handler (->InterruptionHandler :foo :closure-object {:i-metadata :bar})
        restart (->Restart :rname :closure-object {:r-metadata :foo})
        attempt (->Attempt [handler] {:rname restart})]
    (is (instance? Attempt attempt))
    (is (= (:handlers attempt) [handler]))
    (is (= (:restarts attempt) {:rname restart}))))

;; Testing ->interruption function preconditions
(deftest test-interruption-preconditions
  (testing "interruption name field must be a keyword "
    (is (thrown? AssertionError (->interruption nil)))
    (is (thrown? AssertionError (->interruption 'foo)))
    (is (thrown? AssertionError (->interruption 'foo nil)))
    (is (thrown? AssertionError (->interruption 123)))
    (is (interruption? (->interruption :foo)))))
