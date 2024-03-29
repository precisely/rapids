(ns rapids.runtime.persistence_test
  (:require [clojure.test :refer :all]
            [rapids.language.pool-ops :refer [->pool pool-id pool? put-in!]]
            [rapids.objects.address :as a]
            [rapids.objects.pool :refer [raw-pool? make-pool]]
            [rapids.objects.run :as r]
            [rapids.objects.stack-frame :as sf]
            [rapids.runtime.core :refer [current-run with-run]]
            [rapids.runtime.persistence :refer :all]
            [rapids.storage.cache :refer [ensure-cached-connection]]
            [rapids.storage.core :as s]
            [rapids.storage.globals :refer [*cache*]]
            [rapids.storage.protocol :refer [freeze-record frozen? thaw-record]]
            [test-helpers :refer :all]
            [taoensso.nippy :as nippy])
  (:import (rapids.objects.run Run)))

(defn make-run-with-bindings [& {:keys [] :as keys}]
  (r/make-run {:stack (list (sf/make-stack-frame (a/->address `foo 1 2) (or keys {}) nil))}))

(deftest ^:unit pool-persistence-test
  (testing "A pool when created is added to the storage"
    (with-test-env-run [pid (atom nil)
                        p (->pool)]                         ; this adds the pool to the cache
      (swap! pid (constantly (pool-id p)))
      (flush-cache!)
      (is (raw-pool? (get-pool-record @pid)))))
  (testing "A run containing a pool can be stored and retrieved"
    (with-test-storage
      (let [pid (atom nil)
            rid (atom nil)]
        (s/ensure-cached-connection
          (let [p (->pool)]                                 ; this adds the pool to the cache
            (with-run (s/cache-insert! (make-run-with-bindings :mypool p))
              (swap! pid (constantly (pool-id p)))
              (swap! rid (constantly (current-run :id))))))
        (s/ensure-cached-connection
          (with-run (s/cache-get! Run @rid)
            (is (pool? (-> (current-run :stack) first :bindings :mypool)))))))))

(deftest ^:unit circular-reference-test
  (s/with-storage (rapids.implementations.in-memory-storage/->in-memory-storage)
    (ensure-cached-connection
      (testing "A run can reference itself and be saved to storage"
        (let [run (s/cache-insert! (make-run-with-bindings))
              run-id (:id run)]

          (.update run #(update % :stack-frame conj
                          (sf/make-stack-frame
                            (a/->address `foo)
                            {:recursive-ref run}
                            nil)))
          (testing "the run isn't in the storage until we flush-cache!"
            (is (nil? (-> (s/current-storage)
                        :records
                        deref
                        (get-in [Run (:id run)]))))

            (flush-cache!)
            (is (frozen? (-> (s/current-storage)
                           :records
                           deref
                           (get-in [Run run-id])))))

          (testing "then the recursively bound run can be retrieved from storage"
            (is (empty? *cache*))                           ; just checking!
            (let [retrieved (s/cache-get! Run run-id)]
              (is (not (empty? *cache*)))
              (let [recursive-run (get-in retrieved [:stack 0 :bindings :recursive-ref])]
                (= run-id (:id recursive-run))))))))))

(deftest disallowed-objects-test
  (testing "persisting a raw run should fail"
    (is (throws-error-output #"Error while freezing object"
          (nippy/freeze (r/make-run)))))
  (testing "persisting a raw pool should fail"
    (is (throws-error-output #"Error while freezing object"
          (nippy/freeze (make-pool 0))))))

(def ^:dynamic *test-var*)
(deftest ^:unit var-persistence-test
  (testing "can freeze Var"
    (is (frozen? (s/freeze #'*test-var*))))
  (testing "can thaw frozen Var"
    (is (var? (s/thaw (s/freeze #'*test-var*))))))

(deftest ^:unit afunction-persistence-test
  (testing "fails when attempting to freeze a function"
    (is (throws-error-output #"function object cannot be frozen"
          (s/freeze identity)))))

(deftest ^:unit persistence-error-test
  (testing "persistence-error throws an exception for :freeze"
    (is (throws-error-output #"Error while freezing object :foo:"
          (persistence-error :foo :freeze))))
  (testing "persistence-error throws an exception for :thaw"
    (is (throws-error-output #"Error while thawing object :foo:"
          (persistence-error :foo :thaw))))
  (testing "persistence-error allows an extra message"
    (is (throws-error-output #"Error while thawing object :foo: extra info :bar"
          (persistence-error :foo :thaw "extra info %s" :bar)))))