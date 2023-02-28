(ns rapids.language.flow-test
  (:require [clojure.test :refer :all]
            [rapids.language.flow :refer :all]
            [test-helpers :refer :all]))

(deftest flow-test
  (testing "it should not be callable outside of a flow"
    (is (throws-error-output #"Invalid context: anonymous flow may only be defined inside of deflow"
          (macroexpand `(flow [] (* 2 2)))))))