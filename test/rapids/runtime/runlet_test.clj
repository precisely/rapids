(ns rapids.runtime.runlet-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [rapids.objects.address :as a]
            [rapids.objects.flow :as flow]
            [rapids.objects.run :as r]
            [rapids.runtime.runlet :refer :all]
            [rapids.objects.stack-frame :as sf]
            [rapids.storage.core :as s :refer [ensure-cached-connection cache-get! cache-insert!
                                               with-storage ensure-connection]]
            [rapids.objects.signals :as signals]
            [rapids.objects.startable :as startable])
  (:import (rapids.objects.run Run)))

(deftest ^:unit RunSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [address (a/->address `foo)
            stack-frame (sf/make-stack-frame address {:myvar :xyzzy} nil)
            run (r/make-run {:state   :running,
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
            (let [loaded-run (s/get-record! Run (:id run))
                  bound-value (-> run :stack first :bindings :myvar)]
              (is (= bound-value :xyzzy))
              (is (= (:state loaded-run) :complete)))))))))

(rapids.language.flow/deflow foo [a] (* a a))

(deftest ^:unit FlowSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [address (a/->address `foo)
            stack-frame (sf/make-stack-frame address {:foo-flow foo} nil)
            parent-run (r/make-run {:state   :running,
                                    :suspend (signals/make-suspend-signal nil nil nil)
                                    :stack   (list stack-frame)})]
        ;; ensure run1 and run2 are saved to the storage
        (ensure-cached-connection
          (cache-insert! parent-run))

        ;; retrieve parent run
        (ensure-connection
          (let [loaded-parent-run (get-run-record (:id parent-run))
                deserialized-foo (-> loaded-parent-run :stack first :bindings :foo-flow)]
            (is (flow/flow? deserialized-foo))
            (is (fn? (:entry-point deserialized-foo)))
            (is (= 9 (startable/call-entry-point foo [3])))))))))