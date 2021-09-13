(ns rapids.runlet-test
  (:require [clojure.test :refer :all]
            [rapids.run :as r]
            [rapids.runlet :refer :all]
            [rapids.stack-frame :as sf]
            [rapids.address :as a]
            [rapids.storage :as s :refer [ensure-cached-connection cache-update! cache-get! cache-create!
                                          with-storage ->in-memory-storage ensure-connection]]
            [rapids.flow :as flow]
            [rapids.signals :as signals])
  (:import (rapids.run Run)))

(deftest ^:unit RunSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [child-run (r/make-run {:state :running})
            address (a/create `foo)
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
(rapids.deflow/deflow foo [a] (* a a))

(deftest ^:unit FlowSerialization
  (with-storage (->in-memory-storage)
    (testing "Run object stored as a binding will reflect the state of the run in the db"
      (let [address (a/create `foo)
            stack-frame (sf/make-stack-frame address {:foo-flow foo} nil)
            parent-run (r/make-run {:state   :running,
                                    :suspend (signals/make-suspend-signal nil nil nil)
                                    :stack   (list stack-frame)})]
        ;; ensure run1 and run2 are saved to the storage
        (ensure-cached-connection
          (cache-create! parent-run))

        ;; retrieve parent run
        (ensure-connection
          (let [loaded-parent-run (s/get-run (:id parent-run))
                deserialized-foo (-> loaded-parent-run :stack first :bindings :foo-flow)]
            (is (flow/flow? deserialized-foo))
            (is (fn? (:entry-point deserialized-foo)))
            (is (= 9 (flow/entry-point foo [3])))))))))