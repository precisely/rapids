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
            [rapids.storage.core :as s :refer [cache-insert! ensure-cached-connection
                                               ensure-connection with-storage]]
            [test-helpers :refer :all]
            [spy.core :as spy]
            [rapids.support.util :as util])
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

(deftest attach-child-run!-test
  (testing "it should check that child-run is a run"
    (is (throws-error-output #"Expecting a run for waiting-run"
          (attach-waiting-run! {} 0))))
  (testing "it should check that the run is running"
    (with-test-env
      (let [completed-run (cache-insert! (r/make-run {:state :complete}))]
        (is (throws-error-output #"run which is not in running state"
              (attach-waiting-run! completed-run 0))))))
  (testing "it should not allow attaching itself as a child-run"
    (with-test-env
      (let [run (cache-insert! (r/make-run {}))]
        (with-run run
          (is (throws-error-output #"current run to itself as a waiting run"
                (attach-waiting-run! run 0)))))))
  (testing "it disallows duplicates"
    (with-test-env
      (let [run1 (cache-insert! (r/make-run {}))
            run2 (cache-insert! (r/make-run {}))]
        (with-run run1
          ;; first time
          (attach-waiting-run! run2 0)
          (is (throws-error-output #"Duplicate attempt to wait"
                ;; second time
                (attach-waiting-run! run2 0))))))))

(deftest set-run-dynamics-test
  (testing "it should not allow setting a dynamic var which has not been bound in the run"
    (with-test-env
      (let [run (cache-insert! (r/make-run {}))]
        (with-run run
          (is (throws-error-output #"Attempt to set! run dynamic"
                ;; second time
                (set-run-dynamic-var #'*print-length* 3))))))))

(deftest enter-binding-body-test
  (testing "it should not pop run bindings if there's an error"
    (with-test-env
      (with-run (cache-insert! (r/make-run {:dynamics [{}]}))
        (with-redefs [pop-run-bindings! (spy/spy)]
          (enter-binding-body #() {} false)
          (is (spy/not-called? pop-run-bindings!))))))
  (testing "it should pop run bindings if there's an error"
    (with-test-env
      (with-run (cache-insert! (r/make-run {:dynamics [{}]}))
        (with-redefs [pop-run-bindings! (spy/spy)]
          (try
            (enter-binding-body #(throw (ex-info "foo" {})) {} false)
            (catch Exception e))
          (is (spy/called-once? pop-run-bindings!)))))))

(deftest interrupt-run!-test
  (testing "Should fail if run is not suspended (state = :running)"
    (with-test-env
      (with-run (cache-insert! (r/make-run {:state :complete}))
        (is (throws-error-output #"not in :running state"
              (interrupt-run!))))))

  (testing "Should fail if run is already interrupted"
    (with-test-env
      (with-run (cache-insert! (r/make-run {:interrupt (util/new-uuid)}))
        (is (throws-error-output #"already interrupted"
              (interrupt-run!)))))))