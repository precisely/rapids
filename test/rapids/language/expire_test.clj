(ns ^:unit rapids.language.expire_test
  (:require [clojure.test :refer :all]
            [test_helpers :refer :all]
            [rapids :refer :all]
            [rapids.storage.core :as s]))

(deflow expiring-flow []
  (listen! :permit "foo" :expires (now) :default :default-input))

(deftest ^:integration ExpireRuns
  (testing "It causes a suspended run to continue with the default value"
    (with-test-env
      (let [run (start! expiring-flow)]
        (is (= (:state run) :running))
        (flush-cache!)
        (let [num-expired (find-and-expire-runs! 10)]
          (flush-cache!)
          (is (= (:state run) :complete))
          (is (= 1 num-expired)))))))
