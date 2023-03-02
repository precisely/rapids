(ns rapids.objects.CurrentContinuationChange-test
  (:use clojure.test
        rapids.objects.CurrentContinuationChange)
  (:import (rapids.objects CurrentContinuationChange)))

;; Test the constructors and the accessors for stack, dynamics and input
(deftest test-ccc-constructors-and-accessors
  (let [stack    (list :a :b :c)
        dynamics (vec [:foo :bar :baz])
        input    {:some :data}
        ccc      (CurrentContinuationChange. stack dynamics input)]
    (is (= stack (.stack ccc)))
    (is (= dynamics (.dynamics ccc)))
    (is (= input (.input ccc)))))


;; Test that we can catch CurrentContinuationChange and retrieve the data from it
(deftest test-ccc-catching
  (let [stack    (list :a :b :c)
        dynamics (vec [:foo :bar :baz])
        input    {:some :data}
        thunk    (fn []
                   (throw (CurrentContinuationChange. stack dynamics input)))
        result   (try
                   (thunk)
                   (catch CurrentContinuationChange e
                     {:stack    (.stack e)
                      :dynamics (.dynamics e)
                      :input    (.input e)}))]
    (is (= stack (:stack result)))
    (is (= dynamics (:dynamics result)))
    (is (= input (:input result)))))