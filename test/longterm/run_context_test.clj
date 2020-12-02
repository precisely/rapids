(ns longterm.run-context-test
  (:require [clojure.test :refer :all]
            [longterm.run :as r]
            [longterm.run-context :refer :all]
            [longterm.signals :as s]
            [longterm.stack-frame :as sf]
            [longterm.address :as a]
            [longterm.runstore :as rs]
            [longterm.flow :as flow]))

(deftest ^:unit RunSerialization
  (testing "Run object stored as a binding will reflect the state of the run in the db"
    (let [child-run   (r/make-run {:state :suspended})
          address     (a/create `foo)
          stack-frame (sf/make-stack-frame address {:child-run child-run} nil)
          parent-run  (r/make-run {:state :suspended,
                                   :stack (list stack-frame)})]
      ;; ensure run1 and run2 are saved to the runstore
      (with-run-context [parent-run]
        (cache-run! child-run)
        (cache-run! parent-run))

      ;; alter child-run (change state)
      (with-run-context [child-run]
        (cache-run! (assoc child-run :state :complete)))

      ;; retrieve parent run
      (let [loaded-parent-run (rs/get-run (:id parent-run))
            child-run (-> loaded-parent-run :stack first :bindings :child-run)]
        (is (= (:state child-run) :complete))))))

(longterm.deflow/deflow foo [a] (* a a))

(deftest ^:unit FlowSerialization
  (testing "Run object stored as a binding will reflect the state of the run in the db"
    (let [address     (a/create `foo)
          stack-frame (sf/make-stack-frame address {:foo-flow foo} nil)
          parent-run  (r/make-run {:state :suspended,
                                   :stack (list stack-frame)})]
      ;; ensure run1 and run2 are saved to the runstore
      (with-run-context [parent-run]
        (cache-run! parent-run))

      ;; retrieve parent run
      (let [loaded-parent-run (rs/get-run (:id parent-run))
            deserialized-foo (-> loaded-parent-run :stack first :bindings :foo-flow)]
        (is (flow/flow? deserialized-foo))
        (is (fn? (:entry-point deserialized-foo)))
        (is (= 9 (flow/entry-point foo [3])))))))