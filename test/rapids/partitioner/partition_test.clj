(ns rapids.partitioner.partition-test
  (:require [clojure.test :refer :all]
            [matchure.core :refer :all]
            [rapids.partitioner.partition :refer :all]
            [rapids.objects.address :as a]))

(def addr1 (a/->address `foo 0))
(deftest partition-test
  (testing "partition-fn-def"
    (is (if-match [['clojure.core/fn _ [{:keys ['a 'b]}]
                    ['* 'a 'b]]

                   (partition-fn-def {addr1 (->partition '[a b] '[(* a b)])} addr1 0)]
          true))))
