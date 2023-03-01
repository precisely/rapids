(ns rapids.runtime.expire-tests
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.runtime.expire :refer :all]
            [spy.core :as spy]
            [rapids.objects.run :as r]
            [rapids.storage.core :as s]
            [taoensso.timbre :as log]))

(deftest find-and-expire-runs-test
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
      (binding [log/*config* (assoc log/*config* :min-level :report)]
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
                   (is (= 1 result)))))))))
    (testing "when an error occurs while getting runs"
      (binding [log/*config* (assoc log/*config* :min-level :report)]
        (with-test-env
          (with-redefs-fn {#'get-expired-runs (spy/mock (fn [& any] (throw (ex-info "Simulated error while getting expired runs" {}))))
                           #'expire-run!      (spy/spy)}
            #(let [result (find-and-expire-runs! 2)]
               (testing "should return the number of runs processed, which is none"
                 (is (zero? result)))
               (testing "should not call expire-run! since no runs were returned"
                 (is (spy/not-called? expire-run!))))))))))