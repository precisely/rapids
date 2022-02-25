(ns rapids.partitioner.partition-set-test
  (:require [clojure.test :refer :all]
            [matchure.core :refer :all]
            [rapids.objects.address :as address]
            [rapids.partitioner.partition-set :refer :all]
            [rapids.support.util :refer [qualify-symbol]]))

(declare main)                ; get rid of symbol resolution warnings

(deftest ^:unit PartitionSet
  (let [addr  (address/->address `main)
        addr1 (address/child addr 1)
        addr2 (address/child addr 2)
        addr3 (address/child addr 3)
        pset1 (add (create) addr1 [] '[1])
        pset2 (add (create) addr2 [] '[2])
        pset3 (add (create) addr3 [] '[3])]

    (testing "combine"
      (testing "takes nil as argument"
        (is (= (combine nil pset2) pset2))
        (is (= (combine pset1 nil) pset1))
        (is (= (combine nil nil) nil)))

      (testing "addresses of 2 psets"
        (let [combination (combine pset1 pset2)]
          (is (map? combination))
          (is (= (set (keys (addresses combination)))
                (set [addr1 addr2])))))

      (testing "addresses of 3 psets"
        (let [combination (combine pset1 pset2 pset3)]
          (is (map? combination))
          (is (= (set (keys (addresses combination)))
                (set [addr1 addr2 addr3]))))))

    #_(testing "partition-fn-def"
      (is (if-match [[?name ['clojure.core/defn ?fname [{:keys ['a 'b]}]
                             ['clojure.core/binding [] ['* 'a 'b]]]]
                     (partition-fn-def `foo (->Partition '[a b] '[(* a b)]))]
            (= name fname))))

    (testing "partition-fn-set-def"
      (is (if-match [[[['clojure.core/defn ?f1 [{:keys ['a 'b]}] ['clojure.core/binding [] ['* 'a 'b]]]
                       ['clojure.core/defn ?f2 [{:keys ['a 'c]}] ['clojure.core/binding [] ['+ 'c 'a]]]]
                      ['clojure.core/hash-map
                       ?a1 ['quote ?f1-sym]
                       ?a2 ['quote ?f2-sym]]]
                     (partition-fn-set-def {addr1 (->Partition '[a b] '[(* a b)])
                                            addr2 (->Partition '[a c] '[(+ c a)])})]
            (and
              (= a1 addr1)
              (= a2 addr2)
              (= (qualify-symbol f1) f1-sym)
              (= (qualify-symbol f2) f2-sym)))))


    (testing "forced and unforced addresses"
      (let [full-pset   (create)
            full-pset   (add full-pset addr1 '[keep] '[this] true)
            full-pset   (add full-pset addr2 '[drop] '[this])
            forced-pset (remove-unforced full-pset)]
        (is (contains? full-pset addr1))
        (is (contains? full-pset addr2))
        (is (contains? forced-pset addr1))
        (is (not (contains? forced-pset addr2)))))))
