(ns rapids.runtime.calling-test
  (:require [rapids.runtime.calling :refer :all]
            [clojure.test :refer :all]
            [spy.core :as spy])
  (:import (clojure.lang ExceptionInfo)))

(deftest universal-calling-test
  (testing "It should throw an error when the object is not callable"
    (is (thrown-with-msg? ExceptionInfo #"Attempt to call object"
          (universal-call "foo" [1])))))

(deftest fapply-test
  (testing "It should call universal call with various arguments"
    (let [spy+ (spy/spy +)]
      (fapply spy+)
      (fapply spy+ [1])
      (fapply spy+ 1[ 2])
      (fapply spy+ 1 2[3])
      (fapply spy+ 1 2 3 [ 4])
      (fapply spy+ 1 2 3 4 [ 5])
      (fapply spy+ 1 2 3 4 [5 6 7])
      (is (= (spy/calls spy+) '[nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6 7)])))))