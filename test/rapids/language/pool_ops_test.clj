(ns rapids.language.pool-ops-test
  (:require [clojure.test :refer :all]
            [rapids :refer [with-storage current-run]]
            [rapids.implementations.in-memory-storage :refer :all]
            [rapids.objects.signals :refer [suspend-signal?]]
            [rapids.runtime.runlet :refer [with-run]]
            [rapids.objects.run :as r]
            [rapids.storage.core :as s]
            [rapids.language.pool-ops :refer :all]
            [spy.core :as spy])
  (:import (rapids.objects.pool PutIn)))

(defmacro with-env [[& bindings] & body]
  `(with-storage (->in-memory-storage)
     (s/ensure-cached-connection
       (with-run (s/cache-create! (r/make-run))
       (let [~@bindings]
           ~@body)))))

(deftest ^:unit PoolTests
  (testing "An unbuffered pool,"
    (with-env [p (->pool)
               run-id (current-run :id)]
      (testing "should suspend on put-in"
        (is (suspend-signal? (put-in! p :foo))))
      (testing "after suspending on put-in!, a single entry should be in :sources"
        (let [put-in-data (-> @p :sources peek)]
          (is (instance? PutIn put-in-data))
          (testing "the entry should contain the current run id "
            (is (= (:run-id put-in-data) run-id)))
          (testing "the entry should contain the value the put in"
            (is (= (:value put-in-data) :foo)))))
      (testing "take-out! should unblock the run which was blocked by put-in! by..."
        (let [stub (spy/stub :bar)]
          (with-redefs [rapids.runtime.run-loop/continue! stub]
            (testing "returning the value put in"
              (is (= (take-out! p) :foo)))
            (testing "continue! is called once with the blocked run id, no data and the pool-id as permit"
              (is (spy/called-once-with? stub               ; NB: called-once-with? requires exact argument match
                    run-id {:permit (pool-id p)})))))))))   ; i.e., data not provided