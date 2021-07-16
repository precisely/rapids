(ns rapids.run-context-test
  (:require [clojure.test :refer :all]
            [rapids.run :as r]
            [rapids.runlet :refer :all]
            [rapids.signals :as s]
            [rapids.stack-frame :as sf]
            [rapids.address :as a]
            [rapids.storage :as storage]
            [rapids.flow :as flow]
            [rapids.signals :as signals]))

(deftest ^:unit RunSerialization
  (testing "Run object stored as a binding will reflect the state of the run in the db"
    (let [child-run   (r/make-run {:state :suspended})
          address     (a/create `foo)
          stack-frame (sf/make-stack-frame address {:child-run child-run} nil)
          parent-run  (r/make-run {:state :suspended,
                                   :suspend (signals/make-suspend-signal nil nil nil)
                                   :stack (list stack-frame)})]
      ;; ensure run1 and run2 are saved to the storage
      (with-runlet-context [parent-run]
        (cache-run! child-run)
        (cache-run! parent-run))

      ;; alter child-run (change state)
      (with-runlet-context [child-run]
        (cache-run! (assoc child-run :suspend nil, :state :complete)))

      ;; retrieve parent run
      (let [loaded-parent-run (storage/get-run (:id parent-run))
            child-run (-> loaded-parent-run :stack first :bindings :child-run)]
        (is (= (:state child-run) :complete))))))

(rapids.deflow/deflow foo [a] (* a a))

(deftest ^:unit FlowSerialization
  (testing "Run object stored as a binding will reflect the state of the run in the db"
    (let [address     (a/create `foo)
          stack-frame (sf/make-stack-frame address {:foo-flow foo} nil)
          parent-run  (r/make-run {:state   :suspended,
                                   :suspend (signals/make-suspend-signal nil nil nil)
                                   :stack   (list stack-frame)})]
      ;; ensure run1 and run2 are saved to the storage
      (with-runlet-context [parent-run]
        (cache-run! parent-run))

      ;; retrieve parent run
      (let [loaded-parent-run (storage/get-run (:id parent-run))
            deserialized-foo (-> loaded-parent-run :stack first :bindings :foo-flow)]
        (is (flow/flow? deserialized-foo))
        (is (fn? (:entry-point deserialized-foo)))
        (is (= 9 (flow/entry-point foo [3])))))))