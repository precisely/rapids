(ns rapids.objects.interruptions-test
  (:require [clojure.test :refer :all]
            [rapids.objects.interruptions :refer :all])
  (:import (rapids.objects.interruptions Interruption InterruptionHandler Restart Attempt)))

;; Testing Interruption record and ->interruption function
(deftest test-interruption
  (let [interruption (->interruption :foo :message "bar" :data {})]
    (is (instance? Interruption interruption))
    (is (= (:name interruption) :foo))
    (is (= (:message interruption) "bar"))
    (is (= (:data interruption) {}))
    (is (= (:restarts interruption) {}))))

;; Testing StopInterruption and interruption? function
(deftest test-stop-interruption
  (is (interruption? StopInterruption))
  (is (= (:name StopInterruption) :stop))
  (is (= (:message StopInterruption) "The run was stopped"))
  (is (= (:data StopInterruption) nil))
  (is (= (:restarts StopInterruption) {})))

;; Testing InterruptionHandler record
(deftest test-interruption-handler
  (let [handler (->InterruptionHandler :foo :bar)]
    (is (instance? InterruptionHandler handler))
    (is (= (:name handler) :foo))
    (is (= (:flow handler) :bar))))

;; Testing Restart record
(deftest test-restart
  (let [restart (->Restart :foo :bar "baz" {})]
    (is (instance? Restart restart))
    (is (= (:name restart) :foo))
    (is (= (:continuation restart) :bar))
    (is (= (:description restart) "baz"))
    (is (= (:data restart) {}))))

;; Testing Attempt record
(deftest test-attempt
  (let [handler (->InterruptionHandler :foo :bar)
        restart (->Restart :baz :qux "quux" {})
        attempt (->Attempt [handler] {:baz restart})]
    (is (instance? Attempt attempt))
    (is (= (:handlers attempt) [handler]))
    (is (= (:restarts attempt) {:baz restart}))))

;; Testing ->interruption function preconditions
(deftest test-interruption-preconditions
  (is (thrown? AssertionError (->interruption nil)))
  (is (thrown? AssertionError (->interruption :foo :message 123))))
