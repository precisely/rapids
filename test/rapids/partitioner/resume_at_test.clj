(ns rapids.partitioner.resume-at-test
  (:require [rapids.partitioner.resume-at :refer :all]
            [clojure.test :refer :all]
            [rapids.objects.address :as a]))

(deftest ^:unit resume-at-test
  (let [addr      (a/->address `myflow)
        body      '[(foo) (bar)]
        input-key 'result
        params    '[a b]
        expr `(resume-at [~addr ~params ~input-key] ~@body)]
    (testing "resume-at-expr?"
      (is (= true (resume-at-form? `(resume-at))))
      (is (= true (resume-at-form? expr)))
      (is (= false (resume-at-form? `(foo)))))

    (testing "resume-at-data"
      (let [data (resume-at-data expr)]
        (is (= addr (:address data)))
        (is (= params (:params data)))
        (is (= input-key (:input-key data)))
        (is (= body (:body data)))))

    (testing "redirect-resume-at"
      (let [other-address (a/->address `other 123)]
        (is (= other-address
              (-> (redirect-resume-at expr other-address)
                resume-at-data
                :address)))))))

