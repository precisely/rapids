(ns rapids.runtime.persistence_test
  (:require [clojure.test :refer :all]
            [rapids.runtime.persistence :refer :all]
            [rapids.language.pool-ops :refer [->pool put-in! pool-id pool?]]
            [rapids.objects.pool :refer [raw-pool?]]
            [rapids.storage.protocol :refer [thaw-record freeze-record]]
            [helpers :refer :all]
            [rapids.runtime.core :refer [with-run current-run]]
            [rapids.objects.run :as r]
            [rapids.objects.stack-frame :as sf]
            [rapids.objects.address :as a]
            [rapids.storage.core :as s])
  (:import (rapids.objects.run Run)))

(defn make-run-with-bindings [& {:keys [] :as keys}]
  (r/make-run {:stack (list (sf/make-stack-frame (a/->address `foo 1 2) keys nil))}))

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
            (let [p (-> (current-run :stack) first :bindings :mypool)]
              (println "P=" p)
              (is (pool? p)))))))))