(ns ^:unit longterm.expire_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]))

(deflow expiring-flow []
  (listen! :permit :foo :expires (-> 3 days from-now) :default :foo))

(deftest ExpireRun
  (testing "It causes a suspended run to continue with the default value"
    (let [run     (start! expiring-flow)
          expired (expire-run! run)]
      (is (run-in-state? run :suspended))
      (is (run-in-state? expired :complete))
      (is (= (:result expired) :foo)))))
