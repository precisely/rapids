(ns ^:unit rapids.language.expire_test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]))

(deflow expiring-flow []
  (listen! :permit "foo" :expires (-> 3 days from-now) :default :default-data))

(deftest ExpireRun
  (testing "It causes a suspended run to continue with the default value"
    (let [run     (start! expiring-flow)
          expired (expire-run! (:id run))]
      (is (run-in-state? run :running))
      (is (run-in-state? expired :complete))
      (is (= (:result expired) :default-data)))))
