(ns rapids.runtime.persistence_test
  (:require [clojure.test :refer :all]
            [rapids.runtime.persistence :refer :all]
            [rapids.language.pool-ops :refer [->pool put-in! pool-id pool?]]
            [rapids.objects.pool :refer [raw-pool?]]
            [rapids.storage.protocol :refer [thaw-record freeze-record frozen?]]
            [test_helpers :refer :all]
            [rapids.runtime.core :refer [with-run current-run]]
            [rapids.objects.run :as r]
            [rapids.objects.stack-frame :as sf]
            [rapids.storage.cache :refer [ensure-cached-connection]]
            [rapids.objects.address :as a]
            [rapids.storage.core :as s]
            [rapids.storage.dynamics :refer [*cache*]])
  (:import (rapids.objects.run Run)))

(defn make-run-with-bindings [& {:keys [] :as keys}]
  (r/make-run {:stack (list (sf/make-stack-frame (a/->address `foo 1 2) (or keys {}) nil))}))

(deftest ^:unit PoolPersistenceTest
  (testing "A pool when created is added to the storage"
    (with-test-storage
      (let [pid (atom nil)]
        (with-runtime-env [p (->pool)]                      ; this adds the pool to the cache
          (swap! pid (constantly (pool-id p))))
        (with-runtime-env []
          (is (raw-pool? (get-pool @pid)))))))
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

(deftest ^:unit CircularReferenceTest
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
