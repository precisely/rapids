(ns longterm.run_test
  (:require [clojure.test :refer :all]
            [longterm.run :refer :all]
            [java-time :refer [local-date-time]]
            [longterm.signals :as signals]
            [longterm.stack-frame :as sf]
            [clojure.spec.alpha :as s]
            [longterm.address :as a])
  (:import (java.util UUID)))

(deftest ^:unit RunTest
  (s/check-asserts true)
  (testing "can make a new run"
    (is (run? (make-run))))

  (testing "run-to-record"
    (testing "it returns a record when given a minimal run to"
      (let [rec (run-to-record (make-run))]
        (is (map? rec))
        (is (-> rec :stack frozen?))
        (is (-> rec :response frozen?))
        (is (-> rec :run_response frozen?))))

   (testing "a simple run can be turned into a record and back"
      (let [run (make-run)]
        (is (= (-> run run-to-record run-from-record) run))))

    (testing "it correctly transforms a suspended run with all fields set"
      (let [now (local-date-time)
            run (make-run ; fill every field of the run
                  {:state :suspended
                  :stack (list (sf/make-stack-frame (a/create `foo 1 2) {:b 2} 'data-key))
                  :suspend (signals/make-suspend-signal :foo now {:a 1})
                  :run-response ["hello" "there"]
                  :response [:hello :there]
                  :return-mode :redirect
                  :parent-run-id (UUID/randomUUID)
                  :next-id (UUID/randomUUID)
                  :error (Exception. "foo")})
            processed-run (-> run run-to-record run-from-record)]
        (letfn [(same? [k] (= (k run) (k processed-run)))]
          (is (= (same? :state)))
          (is (= (same? :stack)))
          (is (= (same? :suspend)))
          (is (= (same? :response)))
          (is (= (same? :run-response)))
          (is (= (same? :return-mode)))
          (is (= (same? :parent-run-id)))
          (is (= (same? :next-id)))
          (is (= (same? :error))))))))
