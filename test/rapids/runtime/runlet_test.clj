(ns rapids.runtime.runlet-test
  (:require [clojure.test :refer :all]
            [helpers :refer :all]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [rapids.objects.address :as a]
            [rapids.objects.flow :as flow]
            [rapids.objects.run :as r]
            [rapids.runtime.runlet :refer :all]
            [rapids.objects.stack-frame :as sf]
            [rapids.storage.core :as s :refer [ensure-cached-connection cache-update! cache-get! cache-create!
                                               with-storage ensure-connection]]
            [rapids.objects.signals :as signals]
            [rapids.objects.startable :as startable])
  (:import (rapids.objects.run Run)))

(deftest ^:unit RunSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [child-run (r/make-run {:state :running})
            address (a/->address `foo)
            stack-frame (sf/make-stack-frame address {:child-run child-run} nil)
            parent-run (r/make-run {:state   :running,
                                    :suspend (signals/make-suspend-signal nil nil nil)
                                    :stack   (list stack-frame)})]

        ;; ensure run1 and run2 are saved to the storage
        (s/ensure-connection
          (s/create-record! child-run)
          (s/create-record! parent-run))
        ;; alter child-run (change state)
        (s/ensure-connection
          (s/update-record! (assoc child-run :suspend nil, :state :complete))
          ;; retrieve parent run
          (let [loaded-parent-run (s/get-record! Run (:id parent-run))
                child-run (-> loaded-parent-run :stack first :bindings :child-run)]
            (is (= (:state child-run) :complete))))))))
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
          (cache-create! parent-run))

        ;; retrieve parent run
        (ensure-connection
          (let [loaded-parent-run (get-run (:id parent-run))
                deserialized-foo (-> loaded-parent-run :stack first :bindings :foo-flow)]
            (is (flow/flow? deserialized-foo))
            (is (fn? (:entry-point deserialized-foo)))
            (is (= 9 (startable/call-entry-point foo [3])))))))))