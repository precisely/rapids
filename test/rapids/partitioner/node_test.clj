(ns rapids.partitioner.node-test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.node :refer :all]
            [rapids.objects.address :refer :all]
            [rapids.partitioner.partition :as p]
            [rapids.partitioner.resume-at :as r]))
(use 'debux.core)
(def addr (->address `top))
(def addr0 (->address `foo 0))
(def addr1 (->address `foo 1))
(def addr2 (->address `foo 2))
(def addr3 (->address `foo 3))
(def addr4 (->address `foo 4))
(def addr5 (->address `foo 5))
(def addr6 (->address `foo 6))
(def addr7 (->address `foo 7))
(def addr8 (->address `foo 8))
(def addr9 (->address `foo 9))
(def addr10 (->address `foo 10))


(deftest true-address-test
  (testing "true-address"
    (testing "retrieves :start address from a node"
      (is (= addr (true-address (->Node addr nil {}) :start))))
    (testing "retrieves :value address from a node"
      (is (= addr (true-address (->Node nil addr {}) :value))))
    (testing "when provided :tail, retrieves the :start address when no :value address is available"
      (is (= addr0 (true-address (->Node addr0 nil {}) :tail))))
    (testing "when provided :tail, retrieves the :value address when a :value address is available"
      (is (= addr1 (true-address (->Node addr0 addr1 {}) :tail))))
    (testing "fails when an invalid symbolic address is provided"
      (is (thrown? AssertionError (true-address (->Node addr0 addr1 {}) :foo))))))

(deftest ->simple-node-test
  (testing "creating a node with ->simple-node"
    (let [sn (->valued-node addr '[a] '[(* a a)])]
      (testing "it has the same start and value address"
        (is (address? (:start sn)))
        (is (= (:start sn) (:value sn))))
      (testing "the start partition should have the expected params and body"
        (let [start-partition (get-partition sn :start)]
          (is (p/partition? start-partition))
          (is (= '[a] (:params start-partition)))
          (is (= '[(* a a)] (:body start-partition)))))
      (testing "it should return truthy for has-tail-value?"
        (is (has-tail-value? sn)))
      (testing "the start partition should be :valued"
        (is (= :valued (:type (get-partition sn :start))))))))

(deftest ->suspending-node-test
  (testing "creating a suspending node with ->suspending-node"
    (let [sn (->suspending-node addr '[a] '[(<*)])]
      (testing "it should be suspending"
        (is (= :suspending (:type (get-partition sn :start)))))
      (testing "only the start partition should be set"
        (is (p/partition? (get-partition sn :start)))
        (is (nil? (get-partition sn :value)))))))

(deftest has-tail-value?-test
  (testing "has-tail-value?"
    (testing "it should be false whenever the tail partition is not simple"
      (let [start-suspending (->Node addr0 nil {addr0 (p/->partition [] [] :suspending)})
            value-suspending (->Node addr addr0 {addr0 (p/->partition [] [] :suspending)})
            ;; technically should not be possible:
            value-resumed    (->Node addr addr0 {addr0 (p/->partition [] [] :resumed)})]
        (is (false? (has-tail-value? start-suspending)))
        (is (false? (has-tail-value? value-suspending)))
        (is (false? (has-tail-value? value-resumed)))))

    (testing "it should be true whenever the tail partition is simple"
      (let [start-simple (->Node addr0 nil {addr0 (p/->partition [] [] :valued)})
            value-simple (->Node addr addr0 {addr0 (p/->partition [] [] :valued)})]
        (is (true? (has-tail-value? start-simple)))
        (is (true? (has-tail-value? value-simple)))))))

(deftest ensure-tail-value-test
  (testing "adds a value partition to a node which doesn't have one, by default, adding a simple symbol"
    (let [bn (ensure-tail-value (->suspending-node addr [] '[(<*)]))]
      (is (p/partition? (get-partition bn :value)))
      (is (has-tail-value? bn))
      (is (simple-symbol? (p/body-value-expr (get-partition bn :value))))

      (testing "the start partition should be resuming at the value partition"
        (let [rexpr (first (:body (get-partition bn :start)))]
          (is (r/resume-at-expr? rexpr))
          (testing "and the address is the value partition"
            (let [{address :address, input-key :input-key} (r/resume-at-expr-data rexpr)]
              (is (= address (true-address bn :value)))
              (testing "and the input key is provided as the value of the value partition"
                (is (= input-key (p/body-value-expr (get-partition bn :value)))))))))))

  (testing "can be provided a symbol to bind the suspending expression to, which is used as a the value of the value partition"
    (let [bn (ensure-tail-value (->suspending-node addr [] '[(<*)]) 'foo)]
      (is (= (p/body-value-expr (get-partition bn :value))
            'foo))))

  (testing "does not provide a parameter to the value partition"
    (let [bn (ensure-tail-value (->suspending-node addr [] '[(<*)]) false)]
      (is (= (:body (get-partition bn :value)) []))))

  (testing "merely returns the node if the tail partition is already simple"
    (let [node (->Node addr nil {addr (p/->partition [] [] :valued)})]
      (is (= node (ensure-tail-value node)))))

  (testing "fails if the tail partition is already resumed"
    (is (thrown? AssertionError (ensure-tail-value (->Node addr nil {addr (p/->partition [] [] :resumed)}))))))

(deftest chain-test
  (testing "The chain method should"
    (testing "combine the bodies of valued nodes, returning a node with"
      (let [cn (chain (->valued-node addr0 '[a] '[(w) (x)]) (->valued-node addr1 '[a] '[(y) (z)]))]
        (testing "a single partition"
          (is (= (size cn) 1)))
        (testing "a concatenated body"
          (is (= (:body (get-partition cn :value))
                '[(w) (x) (y) (z)])))))

    (testing "combining two suspending nodes,"
      (let [n1 (->Node addr0 addr2 {addr0 (p/->partition '[] '[] :resumed)
                                    addr1 (p/->partition '[] '[] :resumed)
                                    addr2 (p/->partition '[] '[(a)] :valued)})
            n2 (->Node addr3 addr5 {addr3 (p/->partition '[] '[(b)] :resumed)
                                    addr4 (p/->partition '[] '[] :resumed)
                                    addr5 (p/->partition '[] '[] :valued)})
            cn (chain n1 n2)]
        (testing "the tail partition of a suspending partition with the start partition of the second node"
          (is (= (:body (get-partition cn addr2)) '[(a) (b)])))

        (testing "the partitions should be combined in the returned node, but the start partition of the second node should be removed"
          (is (= (size cn) 5))
          (is (= (set (addresses cn)) #{addr0 addr1 addr2 addr4 addr5})))))

    (testing "fail if the params don't match"
      (is (thrown? AssertionError (chain (->valued-node addr0 '[a] '[]) (->valued-node addr1 '[b] '[]))))
      (is (thrown? AssertionError (chain (->Node addr0 addr1 {addr1 (p/->partition '[a] [] :valued)}) (->valued-node addr1 '[b] '[])))))

    (testing "fail if the tail partition of the first node isn't :valued"
      (is (thrown? AssertionError (chain (->suspending-node addr0 '[a] '[]) (->valued-node addr1 '[b] '[])))))))

(deftest bind-test
  (testing "bind, given"
    (let [valued-n1  (->valued-node addr1 '[a] '[(valued1)])
          valued-n2  (->valued-node addr2 '[a] '[(valued2)])
          valued-n3  (->valued-node addr3 '[a] '[(valued3)])
          susp-n4    (->suspending-node addr4 '[a] '[(suspending4)])
          complex-n4 (->Node addr5 addr7 {addr5 (p/->partition '[a] '[(complex-suspending)] :resumed) ; in reality would contain resume-at
                                          addr6 (p/->partition '[a <<99>>] '[(complex-suspending-internal)] :resumed) ; in reality would contain resume-at
                                          addr7 (p/->partition '[a <99>> <<100>>] '[(complex-tail)] :valued)})]

      (testing "a :valued body with no nodes,"
        (let [valued (bind addr '[a] [] [] (fn [syms bindings] `[(~'bar ~syms ~bindings)]) :valued)]
          (testing "should produce a valued node"
            (is (node? valued))
            (is (= (size valued) 1))
            (is (= (atomic? valued))))
          (let [vp (get-partition valued :value)]
            (testing "containing a body generated from empty symbols and bindings vectors"
              (is (= (-> vp p/body-value-expr) '(bar [] [])))
              (testing "at the address passed to bind"
                (is (= addr (true-address valued :value)))))
            (testing "and the params given to the bindings form"
              (is (= '[a] (:params vp)))))))

      (testing "a :valued body with valued arguments,"
        (let [node (bind addr '[foo] '[s1 s2] [valued-n1 valued-n2] (fn [syms bindings] `[{:syms ~syms :bindings ~bindings}]) :valued)]
          (testing "should return a valued node"
            (is (node? node))
            (is (= (size node) 1))
            (is (= (atomic? node)))
            (let [vp (get-partition node :value)]
              (testing "containing a body generated from empty symbols and bindings vectors"
                (is (= (-> vp p/body-value-expr) '{:syms [], :bindings [[s1 (valued1)] [s2 (valued2)]]})))
              (testing "and the params given to the bindings form"
                (is (= '[foo] (:params vp))))))))

      (testing "a :valued body with several non-suspending bindings ending with a suspending binding, should return"
        (let [node   (bind addr '[foo] '[s1 s2 s3] [valued-n1 valued-n2 susp-n4] (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) :valued)
              startp (get-partition node :start)
              valuep (get-partition node :value)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "where the value partition is valued"
              (is (= :valued (:type valuep))))
            (testing "where the start partition is resumed"
              (is (= :resumed (:type startp)))
              (testing "and contains a let expr where"
                (let [[op bindings ra-expr & remaining-body] (-> startp :body first)]
                  (is (= op 'clojure.core/let))
                  (testing "the first 2 symbols are bound"
                    (is (= (map first (partition 2 bindings)) '[s1 s2])))
                  (testing "the tail expr resumes at the value address"
                    (is (r/resume-at-expr? ra-expr))
                    (let [{a :address, i :input-key, b :body, p :params}
                          (r/resume-at-expr-data ra-expr)]
                      (is (= (:value node) a))
                      (is (= p '[foo s1 s2]))
                      (is (= i 's3))
                      (is (= b '[(suspending4)]))
                      (is (empty? remaining-body)))))))
            (testing "where the value partition is valued"
              (is (= :resumed (:type startp)))))))

      (testing "a :valued body with a suspending binding followed by a non suspending binding, should return"
        (let [node   (bind addr '[foo] '[s1 s2 s3] [valued-n1 susp-n4 valued-n2] (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) :valued)
              startp (get-partition node :start)
              valuep (get-partition node :value)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "where the value partition is valued"
              (is (= :valued (:type valuep))))
            (testing "where the start partition is resumed"
              (is (= :resumed (:type startp)))
              (testing "and contains a let expr where"
                (let [[op bindings ra-expr & remaining-body] (-> startp :body first)]
                  (is (= op 'clojure.core/let))
                  (testing "the first symbol is bound"
                    (is (= (map first (partition 2 bindings)) '[s1])))
                  (testing "the tail expr resumes at the value address"
                    (is (= (r/resume-at-expr? ra-expr)))
                    (let [{a :address, i :input-key, b :body, p :params}
                          (r/resume-at-expr-data ra-expr)]
                      (testing "but note that the value address is not the given address"
                        (is (not= addr a)))
                      (is (= (:value node) a))
                      (is (= p '[foo s1]))
                      (is (= i 's2))
                      (is (= b '[(suspending4)]))
                      (is (empty? remaining-body)))))))
            (testing "where the value partition"
              (testing "is :valued"
                (is (= :valued (:type valuep))))
              (testing "contains the correct params"
                (is (= '[foo s1 s2] (:params valuep))))
              (testing "contains the expected body"
                (is (= '[{:bindings [[s2 s2] [s3 (valued2)]] :syms [s1]}] (:body valuep))))))))

      (testing "a :valued body with a complex suspending binding (containing several internal partitions) should return,"
        (let [node   (bind addr '[foo] '[s1 s2 s3] [valued-n1 complex-n4 valued-n2] (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) :valued)
              startp (get-partition node :start)
              valuep (get-partition node :value)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "containing the intermediate address(es) of the complex node"
              (is (empty? (clojure.set/difference #{addr6 addr7} (set (addresses node))))))
            (testing "where the value partition is valued"
              (is (= :valued (:type valuep))))
            (testing "where the start partition is resumed"
              (is (= :resumed (:type startp)))
              (testing "and contains a let expr where"
                (let [[op bindings ra-expr & remaining-body] (-> startp :body first)]
                  (is (= op 'clojure.core/let))
                  (testing "the first symbol is bound"
                    (is (= (map first (partition 2 bindings)) '[s1])))
                  (testing "the tail expr resumes at the value address"
                    (is (= (r/resume-at-expr? ra-expr)))
                    (let [{a :address, i :input-key, b :body, p :params}
                          (r/resume-at-expr-data ra-expr)]
                      (testing "but note that the value address is not the given address"
                        (is (not= addr a)))
                      (is (= (:value node) a))
                      (is (= p '[foo s1]))
                      (testing "and the input-key is bound to the correct variable"
                        (is (= i 's2)))
                      (testing "and the start body of the suspending node constitutes the body of the resume expression"
                        (is (= b '[(complex-suspending)])))
                      (testing "and nothing follows the resume expression"
                        (is (empty? remaining-body))))))))
            (testing "where the value partition"
              (testing "is :valued"
                (is (= :valued (:type valuep))))
              (testing "contains the expected parameters"
                (is (= '[foo s1 s2] (:params valuep))))
              (testing "has the expected body"
                (is (= '[{:bindings [[s2 (complex-tail)] [s3 (valued2)]] :syms [s1]}] (:body valuep)))))))))))