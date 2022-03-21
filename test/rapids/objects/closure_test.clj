(ns rapids.objects.closure-test
  (:require [clojure.test :refer :all]
            [rapids.objects.address :as a]
            [rapids.objects.closure :refer :all]
            [rapids.partitioner.closure :refer [closure-constructor]]
            [rapids.partitioner.partition-map :as pmap]
            [rapids.objects.flow :as f]
            [rapids.runtime.calling :refer [universal-call]]
            [spy.core :as spy]
            [clojure.tools.macro :refer [macrolet]]))

(defn closure-fn [& args] args)
(def main (f/->Flow `main (fn [& rest]) {[0] (fn [bindings]
                                               (fn [& args]
                                                 (apply closure-fn bindings args)))}))
(deftest closure-constructor-test
  (let [address            (a/->address `main 0)
        fndef              '(fn [x] (* x y))
        [closure-ctor, pmap] (closure-constructor fndef, address '[y z])
        suspending-closure (->Closure address {:foo 1} true)
        fn-closure         (->Closure address {:foo 1} false)]

    (testing "the first result is an expression which constructs a Closure object with the correct bindings"
      (is (= closure-ctor `(->Closure ~address (hash-map :y ~'y) false))))

    (testing "Named implementation of Closure"
      (is (= (name suspending-closure) (a/to-string address)))
      (is (= (namespace suspending-closure) (namespace `main)))
      (is (string? (closure-name suspending-closure))))

    (testing "should throw an error when a suspending Closure is called at top level"
      (is (thrown-with-msg? Exception #"Attempt to call suspending"
            (suspending-closure))))

    (testing "calling non-suspending closure with multiple arguments"
      (macrolet [(dotest [args]
                   `(with-redefs [closure-fn (spy/stub)]
                      (~'fn-closure ~@args)
                      (spy/called-once-with? closure-fn (:bindings ~'fn-closure) ~@args)))]
        (dotimes [i 22]
          (is (dotest (repeat i i))))))

    (testing "calling suspending closure with multiple arguments"
      (dotimes [i 22]
        (with-redefs [closure-fn (spy/stub)]
          (let [args (repeat i i)]
            (universal-call suspending-closure args)
            (is (apply spy/called-once-with? closure-fn (:bindings fn-closure) args))))))

    (testing "name protocol"
      (is (string? (name suspending-closure)))
      (is (string? (namespace suspending-closure))))

    (testing "the second result is a partition-map which has a value for the address"
      ;; only y is needed, not z which isn't bound by the fn or x which is provided as an argument
      (is (pmap/partition-map? pmap))

      (testing "the partition should contain the function as a body"
        (let [partition (get pmap address)]
          (is (pmap/partition? partition))
          (is (= (:params partition) '[y]))
          (is (= (:body partition) [fndef])))))))