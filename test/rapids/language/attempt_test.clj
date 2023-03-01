(ns rapids.language.attempt-test
  (:require [clojure.test :refer :all]
            [rapids.language.attempt :refer :all]
            [test-helpers :refer :all]))

(deftest attempt-macro-tests
  (testing "attempt handlers"
    (is (throws-error-output #"(?m)First argument to attempt handle clause should be a keyword"
          (macroexpand '(attempt (print "no op")
                          (handle "foo"
                            (print "this should cause an exception"))))))
    (is (throws-error-output #"(?m)Second argument to attempt handle should be an unqualified symbol"
          (macroexpand '(attempt (print "no op")
                          (handle :foo :boo
                            (print "this should cause an exception"))))))))

(deftest normalize-restart-test
  (testing "s-expr style restart"
    (testing "should turn into a map"
      (is (= {:data        nil
              :name        :set-dosage
              :description "description"
              :do          '(flow [a] (do-something a))}
            (normalize-restart-def '(:set-dosage "description" [a] (do-something a)))))))
  (testing "map style restart"
    (testing "valid restart should return as provided"
      (let [valid-restart {:name        :set-dosage
                           :description "description"
                           :do          '(flow [a] (do-something a))}]
        (is (= valid-restart
              (normalize-restart-def
                valid-restart)))))
    (testing "invalid description should error"
      (is (throws-error-output #"(?m)If provided, restart description must be string or function"
            (normalize-restart-def
              {:name        :set-dosage
               :description 'abc
               :do          '(flow [a] (do-something a))}))))
    (testing "Invalid restart name should error"
      (is (throws-error-output #"(?m)Restart name must be a keyword"
            (normalize-restart-def
              {:name        "foo"
               :description "description"
               :do          '(flow [a] (do-something a))})))
      (is (throws-error-output #"(?m)Restart name must be a keyword"
            (normalize-restart-def
              {:name        'foo
               :description "description"
               :do          '(flow [a] (do-something a))}))))
    (testing "Invalid do clause should error"
      (is (throws-error-output #"(?m)Invalid do clause in restart"
            (normalize-restart-def
              {:name        :foo
               :description "description"
               :do          '(fn [a] (do-something a))}))))))

(deftest handle-clause-test
  (testing "Should error when invoked outside of attempt clause"
    (is (throws-error-output #"Attempt handler must appear at end of attempt body"
          (macroexpand '(handle :foo i (do-something)))))))