(ns rapids.runtime.runlet-test
  (:require [clojure.test :refer :all]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [rapids.objects.address :as a]
            [rapids.objects.flow :as flow]
            [rapids.objects.run :as r]
            [rapids.objects.signals :as signals]
            [rapids.objects.stack-frame :as sf]
            [rapids.objects.startable :as startable]
            [rapids.runtime.runlet :refer :all]
            [rapids.storage.core :as s :refer [cache-get! cache-insert! ensure-cached-connection
                                               ensure-connection with-storage]]
            [test-helpers :refer :all])
  (:import (rapids.objects.run Run)))

(deftest ^:unit RunSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [address     (a/->address `foo)
            stack-frame (sf/make-stack-frame address {:myvar :xyzzy} nil)
            run         (r/make-run {:state   :running,
                                     :suspend (signals/make-suspend-signal nil nil nil)
                                     :stack   (list stack-frame)})]

        (testing "run can be saved to storage and retrieved"
          (s/ensure-connection
            (s/create-record! run)
            (is (= run (get-run-record (:id run))))))

        (testing "run can be altered"
          (s/ensure-connection
            (s/update-record! (assoc run :state :complete))
            ;; retrieve run
            (let [loaded-run  (s/get-record! Run (:id run))
                  bound-value (-> run :stack first :bindings :myvar)]
              (is (= bound-value :xyzzy))
              (is (= (:state loaded-run) :complete)))))))))

(rapids.language.flow/deflow foo [a] (* a a))

(deftest ^:unit FlowSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [address     (a/->address `foo)
            stack-frame (sf/make-stack-frame address {:foo-flow foo} nil)
            parent-run  (r/make-run {:state   :running,
                                     :suspend (signals/make-suspend-signal nil nil nil)
                                     :stack   (list stack-frame)})]
        ;; ensure run1 and run2 are saved to the storage
        (ensure-cached-connection
          (cache-insert! parent-run))

        ;; retrieve parent run
        (ensure-connection
          (let [loaded-parent-run (get-run-record (:id parent-run))
                deserialized-foo  (-> loaded-parent-run :stack first :bindings :foo-flow)]
            (is (flow/flow? deserialized-foo))
            (is (fn? (:entry-point deserialized-foo)))
            (is (= 9 (startable/call-entry-point foo [3])))))))))

(deftest with-run-test
  (with-test-env
    (let [r  (cache-insert! (r/make-run))
          r2 (r/make-run)]
      (testing "current run may be set using id"
        (with-run (:id r)
          (is (= (:id r) rapids.runtime.globals/*current-run-id*))))
      (testing "current run may be set using run object"
        (with-run r
          (is (= (:id r) rapids.runtime.globals/*current-run-id*))))
      (testing "should throw an exception if the run doesn't exist"
        (is (throws-error-output #"Object not found"
              (with-run r2 (:id r2))))))))