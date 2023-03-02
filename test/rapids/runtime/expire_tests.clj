(ns rapids.runtime.expire-tests
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.runtime.expire :refer :all]
            [spy.core :as spy]
            [rapids.objects.run :as r]
            [rapids.storage.core :as s]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [taoensso.timbre :as log]
            [net.r4s6.test-async :as ta :include-macros true]))

(deftest find-and-expire-runs-test
  (doseq [min-level (if (System/getenv "CI") [:trace :report] [:report])]
    (binding [log/*config* (assoc log/*config* :min-level min-level)]
      (testing "find-and-expire-runs!"
        (testing "returns zero when no runs are available to expire"
          (with-test-storage (is (zero? (find-and-expire-runs! 0)))))
        (testing "when fewer expired runs exist than the n argument"
          (with-test-env
            (let [[r1 r2] [(s/cache-insert! (r/make-run)) (s/cache-insert! (r/make-run))]]
              (flush-cache!)
              (with-redefs-fn {#'get-expired-runs (spy/stub [r1 r2])
                               #'expire-run!      (spy/spy)}
                #(let [result (find-and-expire-runs! 4)]
                   (testing "should return the number of runs processed, which is the number available"
                     (is (= 2 result)))
                   (testing "should call expire-run! for each run"
                     (is (= `[(~r1) (~r2)] (spy/calls expire-run!)))))))))
        (testing "when more expired runs exist than the n argument"
          (with-test-env
            (let [[r1 r2 _] [(s/cache-insert! (r/make-run)) (s/cache-insert! (r/make-run)) (s/cache-insert! (r/make-run))]]
              (flush-cache!)
              (with-redefs-fn {#'get-expired-runs (spy/stub [r1 r2])
                               #'expire-run!      (spy/spy)}
                #(let [result (find-and-expire-runs! 2)]
                   (testing "should return the number of runs processed, which is n"
                     (is (= 2 result)))
                   (testing "should call expire-run! for each run"
                     (is (= `[(~r1) (~r2)] (spy/calls expire-run!)))))))))
        (testing "when there is an error expiring runs"
          (with-test-env
            (let [[error-run successful-run] [(s/cache-insert! (r/make-run)) (s/cache-insert! (r/make-run))]
                  expire-results (atom [#(throw (ex-info "Simulated failure while expiring run" {})) #()])]
              (flush-cache!)
              (with-redefs-fn {#'get-expired-runs (spy/stub [error-run successful-run])
                               #'expire-run!      (spy/mock (fn [& a]
                                                              (let [f (first @expire-results)
                                                                    _ (swap! expire-results rest)]
                                                                (f))))}
                #(let [result (find-and-expire-runs! 2)]
                   (testing "should call expire-run! for each run, even runs after the error-generating run"
                     (is (= `[(~error-run) (~successful-run)] (spy/calls expire-run!))))
                   (testing "should return the number of runs processed, which is n"
                     (is (= 1 result))))))))
        (testing "when an error occurs while getting runs"
          (with-test-env
            (with-redefs-fn {#'get-expired-runs (spy/mock (fn [& any] (throw (ex-info "Simulated error while getting expired runs" {}))))
                             #'expire-run!      (spy/spy)}
              #(let [result (find-and-expire-runs! 2)]
                 (testing "should return the number of runs processed, which is none"
                   (is (zero? result)))
                 (testing "should not call expire-run! since no runs were returned"
                   (is (spy/not-called? expire-run!)))))))))))

(deftest expiry-monitor-test
  (binding [log/*config* (assoc log/*config* :min-level :report)]
    (testing "starting and stopping the expiry monitor"
      (let [storage (->in-memory-storage)]
        (is (not (contains? *expiry-monitors* storage)))
        (testing "start-expiry-monitor! should add the storage to the expiry monitors"
          (start-expiry-monitor! :storage storage :delay 100)
          (is (contains? *expiry-monitors* storage))
          (testing "and the delay should be recorded appropriately"
            (is (= 100 (get *expiry-monitors* storage))))
          (testing "calling start-expiry-monitor! again changes the delay"
            (start-expiry-monitor! :storage storage :delay 9999)
            (is (= 9999 (get *expiry-monitors* storage))))
          (testing "stop-expiry-monitor! should set the table entry delay to nil"
            (stop-expiry-monitor! storage)
            (is (= nil (get *expiry-monitors* storage))))
          (testing "start-expiry-monitor! and stop-expiry-monitor! default to using the current storage"
            (let [storage (->in-memory-storage)]
              (rapids.storage.core/with-storage storage
                (start-expiry-monitor! :delay 100)
                (is (= 100 (get *expiry-monitors* storage)))
                (stop-expiry-monitor!)
                (is (nil? (get *expiry-monitors* storage)))))))))
    (testing "start-expiry-monitor! should call find-and-expire-runs!"
      (ta/async
        done
        (future
          (let [storage (->in-memory-storage)]
            (with-redefs-fn {#'find-and-expire-runs! (spy/spy)}
              #(do
                 (start-expiry-monitor! :storage storage :delay 0.0001 :n 3)

                 (Thread/sleep 100)
                 (stop-expiry-monitor! storage)
                 (is (spy/called-at-least-once? find-and-expire-runs!))
                 (done)))))))))