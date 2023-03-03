(ns rapids.objects.run-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.objects.run :refer :all]
            [rapids.support.util :refer :all])
  (:import [rapids.objects.run Run]))

(deftest make-run-test
  (testing "it makes a default run"
    (is (instance? Run (make-run))))
  (testing "it makes a run with given values"
    (let [fields     {:id       (new-uuid)
                      :state    :complete
                      :stack    [:foo]
                      :output   ["bar"]
                      :result   123
                      :waits    {(new-uuid) 2}
                      :dynamics [{#'*clojure-version* "1.11.1"}]
                      :index    {:baz 3}}
          run        (make-run fields)
          field-set? #(and (contains? fields %) (= (% fields) (% run)))]
      (is (instance? Run (make-run)))
      (is (field-set? :id))
      (is (field-set? :state))
      (is (field-set? :stack))
      (is (field-set? :output))
      (is (field-set? :result))
      (is (field-set? :dynamics))
      (is (field-set? :index)))))

(deftest valid-run-data?-test
  (testing "it should throw when an invalid key is given"
    (is (throws-error-output #"Invalid key"
          (valid-run-data? (make-run {:wrong-field "foo"})))))
  (testing "it should throw when an invalid data is given"
    (is (throws-error-output #"Invalid data"
          (valid-run-data? (make-run {:id ["wrong!"]}))))))

(deftest get-dynamics-test
  (testing "get-dynamic-value correctly retrieves the value of a dynamic variable in the run"
    (testing "given a var"
      (is (= "1.11.1" (get-dynamic-value (make-run {:dynamics [{#'*clojure-version* "1.10.0"}
                                                               {#'*clojure-version* "1.11.1"}]})
                        #'*clojure-version*))))
    (testing "given a symbol"
      (is (= "1.11.1" (get-dynamic-value (make-run {:dynamics [{#'*clojure-version* "1.10.0"}
                                                               {#'*clojure-version* "1.11.1"}]})
                        '*clojure-version*)))))
  (testing "get-dynamic-values retrieves all values of a dynamic variable, most recent first"
    (is (= ["1.11.1" "1.10.0"] (get-dynamic-values (make-run {:dynamics [{#'*clojure-version* "1.10.0"}
                                                                         {#'*clojure-version* "1.11.1"}]})
                                 #'*clojure-version*))))
  (testing "get-dynamic-values retrieves a default value when variable isn't present"
    (is (= :not-found (get-dynamic-value (make-run {:dynamics []})
                                 #'*clojure-version*
                                 :not-found)))))