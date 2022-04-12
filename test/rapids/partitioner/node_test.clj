(ns rapids.partitioner.node-test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.node :refer :all]
            [rapids.objects.address :refer :all]
            [rapids.partitioner.partition :as p]
            [rapids.partitioner.resume-at :as r]
            [rapids.partitioner.node :as n]
            [rapids.objects.address :as a]))
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
    (testing "retrieves :head address from a node"
      (is (= addr (true-address (->Node addr nil {}) :head))))
    (testing "retrieves :value address from a node"
      (is (= addr (true-address (->Node nil addr {}) :tail))))
    (testing "fails when an invalid symbolic address is provided"
      (is (thrown? AssertionError (true-address (->Node addr0 addr1 {}) :foo))))))

(deftest ->simple-node-test
  (testing "creating a node with ->simple-node"
    (let [sn (->valued-node addr '[a] '[(* a a)])]
      (testing "it has the same start and value address"
        (is (address? (:head sn)))
        (is (= (:head sn) (:tail sn))))
      (testing "the head partition should have the expected params and body"
        (let [start-partition (get-partition sn :head)]
          (is (p/partition? start-partition))
          (is (= '[a] (:params start-partition)))
          (is (= '[(* a a)] (:body start-partition)))))
      (testing "it should return truthy for has-tail-value?"
        (is (has-tail-value? sn)))
      (testing "the head partition should be :valued"
        (is (not (:suspending (get-partition sn :head))))))))

(deftest ->suspending-node-test
  (testing "creating a suspending node with ->suspending-node"
    (let [sn (->suspending-node addr '[a] '[(<*)])]
      (testing "it should be suspending"
        (is (:suspending (get-partition sn :head))))
      (testing "only the head partition should be set"
        (is (p/partition? (get-partition sn :head)))
        (is (:suspending (get-partition sn :tail)))))))

(deftest has-tail-value?-test
  (testing "has-tail-value?"
    (testing "it should be false whenever the tail partition is not simple"
      (let [start-suspending-only (->Node addr0 addr1 {addr0 (p/->partition [] [] true)
                                                       addr1 (p/->partition [] [] false)})
            tail-suspending       (->Node addr addr0 {addr0 (p/->partition [] [] true)})]
        (is (true? (has-tail-value? start-suspending-only)))
        (is (false? (has-tail-value? tail-suspending)))))))

(deftest ensure-tail-value-test
  (testing "adds a tail partition to a node which doesn't have one, by default, with an empty body"
    (let [bn (ensure-bindable (->suspending-node addr [] '[(<*)]))]
      (is (p/partition? (get-partition bn :tail)))
      (is (has-tail-value? bn))
      (is (= 1 (-> (get-partition bn :tail) :body count)))

      (testing "the head partition should be resuming at the tail partition"
        (let [rexpr (first (:body (get-partition bn :head)))]
          (is (r/resume-at-expr? rexpr))
          (testing "and the address is the tail partition"
            (let [{address :address, input-key :input-key} (r/resume-at-expr-data rexpr)]
              (is (= address (true-address bn :tail)))))))))

  (testing "can be provided a symbol to bind the suspending expression to, which is used as a the value of the tail partition"
    (let [bn (ensure-bindable (->suspending-node addr [] '[(<*)]) 'foo)]
      (is (= (p/body-value-expr (get-partition bn :tail))
            'foo))))

  (testing "provides a parameter to the tail partition by default"
    (let [bn (ensure-bindable (->suspending-node addr [] '[(<*)]))]
      (is (simple-symbol? (-> (get-partition bn :tail) :body first)))))

  (testing "merely returns the node if the tail partition is already non-suspending"
    (let [node (->Node addr0 addr1 {addr0 (p/->partition [] [] true)
                                    addr1 (p/->partition [] [] false)})]
      (is (= node (ensure-bindable node))))))

(deftest chain-test
  (testing "The chain method should"
    (testing "combine the bodies of valued nodes, returning a node with"
      (let [cn (chain (->valued-node addr0 '[a] '[(w) (x)]) (->valued-node addr1 '[a] '[(y) (z)]))]
        (testing "a single partition"
          (is (= (size cn) 1)))
        (testing "a concatenated body"
          (is (= (:body (get-partition cn :tail))
                '[(w) (x) (y) (z)])))))

    (testing "combine two suspending nodes by..."
      (let [n1 (->Node addr0 addr2 {addr0 (p/->partition '[a] '[0] true)
                                    addr1 (p/->partition '[a] '[1] true)
                                    addr2 (p/->partition '[a] '[2] true)})
            n2 (->Node addr3 addr5 {addr3 (p/->partition '[a] '[3] true)
                                    addr4 (p/->partition '[a] '[4] true)
                                    addr5 (p/->partition '[a] '[5] true)})
            cn (chain n1 n2)]

        (testing "preserving the internal partitions"
          (is (= (set (n/addresses cn)) #{addr0 addr1 addr2 addr3 addr4 addr5})))

        (testing "resuming the first node's tail partition at the second node's head partition"
          (let [join-expr (first (:body (get-partition cn (:tail n1))))]
            (is (r/resume-at-expr? join-expr))
            (is (= (r/resume-at-expr-data join-expr :address)
                  (:head n2)))
            (is (= (r/resume-at-expr-data join-expr :params)
                  (:params (get-partition n1 :head))))))))

    (testing "fail if the params don't match"
      (is (thrown? AssertionError (chain (->valued-node addr0 '[a] '[]) (->valued-node addr1 '[b] '[]))))
      (is (thrown? AssertionError (chain (->Node addr0 addr1 {addr1 (p/->partition '[a] [] :valued)}) (->valued-node addr1 '[b] '[])))))

    (testing "fail if the tail partition of the first node isn't :valued"
      (is (thrown? AssertionError (chain (->suspending-node addr0 '[a] '[]) (->valued-node addr1 '[b] '[])))))))

(defn test-partitioner [exprf addr params] (exprf addr params))

(deftest bind-test
  (testing "bind, given"
    (let [valued-n1          (fn [addr params] (->valued-node addr params '[(valued1)]))
          valued-n2          (fn [addr params] (->valued-node addr params '[(valued2)]))
          susp-n4            (fn [addr params] (->suspending-node addr params '[(suspending4)]))
          internal-address-1 (->address `internal 1)
          internal-address-2 (->address `internal 2)
          complex-n4         (fn [addr params]
                               (let [head (a/child addr '_head)]
                                 (->Node head internal-address-2
                                   {head               (p/->partition `[~@params] '[(complex-suspending)] true) ; in reality would contain resume-at
                                    internal-address-1 (p/->partition `[~@params ~'intermed-p1] '[(complex-suspending-internal)] true) ; in reality would contain resume-at
                                    internal-address-2 (p/->partition `[~@params ~'intermed-p1 ~'intermed-p2] '[(complex-tail)] false)})))]
      (testing "a non-suspending body with no nodes,"
        (let [valued (bind addr '[a] [] []
                       test-partitioner
                       (fn [syms bindings] `[(~'bar ~syms ~bindings)]) false)]
          (testing "should produce a valued node"
            (is (node? valued))
            (is (= (size valued) 1))
            (is (= (atomic? valued))))
          (let [tp (get-partition valued :tail)]
            (testing "containing a body generated from empty symbols and bindings vectors"
              (is (= (-> tp p/body-value-expr) '(bar [] [])))
              (testing "at the address passed to bind"
                (is (= addr (true-address valued :tail)))))
            (testing "and the params given to the bindings form"
              (is (= '[a] (:params tp)))))))
      (testing "a non-suspending body with valued arguments,"
        (let [node (bind addr '[foo] '[s1 s2] [valued-n1 valued-n2]
                     test-partitioner
                     (fn [syms bindings] `[{:syms ~syms :bindings ~bindings}]) false)]
          (testing "should return a valued node"
            (is (node? node))
            (is (= (size node) 1))
            (is (= (atomic? node)))
            (let [vp (get-partition node :tail)]
              (testing "containing a body generated from empty symbols and bindings vectors"
                (is (= (-> vp p/body-value-expr) '{:syms [], :bindings [[s1 (valued1)] [s2 (valued2)]]})))
              (testing "and the params given to the bindings form"
                (is (= '[foo] (:params vp))))))))

      (testing "a non-suspending body with several non-suspending bindings ending with a suspending binding, should return"
        (let [node  (bind addr '[a] '[s1 s2 s3] [valued-n1 valued-n2 susp-n4]
                      test-partitioner
                      (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) false)
              headp (get-partition node :head)
              tailp (get-partition node :tail)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "where the tail partition is non-suspending"
              (is (not (p/suspending? tailp))))
            (testing "where the head partition is suspending"
              (is (p/suspending? headp))
              (testing "and contains a let expr where"
                (let [[op bindings ra-expr & remaining-body] (-> headp :body first)]
                  (is (= op 'clojure.core/let))
                  (testing "the first 2 symbols are bound"
                    (is (= (map first (partition 2 bindings)) '[s1 s2])))
                  (testing "the body of the head partition contains a resume-at expression"
                    (is (r/resume-at-expr? ra-expr))
                    (let [{a :address, i :input-key, b :body, p :params}
                          (r/resume-at-expr-data ra-expr)]
                      (testing "which resumes at the node's tail address"
                        (is (= (:tail node) a)))
                      (is (= p '[a s1 s2]))
                      (is (= i 's3))
                      (is (= b '[(suspending4)]))
                      (is (empty? remaining-body)))))))
            (testing "where the tail partition"
              (testing "does not suspend"
                (is (not (p/suspending? tailp))))
              (testing "has the correct parameters"
                (is (= '[a s1 s2 s3] (:params tailp))))))))
      (testing "a non-suspending body with a suspending binding followed by a non suspending binding, should return"
        (let [node  (bind addr '[foo] '[s1 s2 s3] [valued-n1 susp-n4 valued-n2]
                      test-partitioner
                      (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) false)
              headp (get-partition node :head)
              tailp (get-partition node :tail)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "where the tail partition is valued"
              (is (not (p/suspending? tailp))))
            (testing "where the head partition is resumed"
              (is (p/suspending? headp))
              (testing "and contains a let expr where"
                (let [[op bindings ra-expr & remaining-body] (-> headp :body first)]
                  (is (= op 'clojure.core/let))
                  (testing "the first symbol is bound"
                    (is (= (map first (partition 2 bindings)) '[s1])))
                  (testing "the tail expr resumes at the value address"
                    (is (= (r/resume-at-expr? ra-expr)))
                    (let [{a :address, i :input-key, b :body, p :params}
                          (r/resume-at-expr-data ra-expr)]
                      (testing "but note that the value address is not the given address"
                        (is (not= addr a)))
                      (is (= (:tail node) a))
                      (is (= p '[foo s1]))
                      (is (= i 's2))
                      (is (= b '[(suspending4)]))
                      (is (empty? remaining-body)))))))
            (testing "where the tail partition"
              (testing "is :valued"
                (is (not (p/suspending? tailp))))
              (testing "contains the correct params"
                (is (= '[foo s1 s2] (:params tailp))))
              (testing "contains the expected body"
                (is (= '[{:bindings [[s2 s2] [s3 (valued2)]] :syms (s1)}] (:body tailp))))))))

      (testing "a non-suspending body with a complex suspending binding (containing several internal partitions) should return,"
        (let [node   (bind addr '[foo] '[s1 s2 s3] [valued-n1 complex-n4 valued-n2]
                       test-partitioner
                       (fn [syms bindings] [`{:syms ~syms :bindings ~bindings}]) false)
              startp (get-partition node :head)
              tailp  (get-partition node :tail)]
          (testing "a suspending node"
            (is (suspends? node))
            (testing "containing the internal address(es) of the complex node"
              (is (empty? (clojure.set/difference #{internal-address-1 internal-address-2} (set (addresses node))))))
            (testing "where the tail partition is valued"
              (is (not (p/suspending? tailp))))
            (testing "where the head partition is resumed"
              (is (p/suspending? startp))
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
                      (is (= (:tail node) a))
                      (is (= p '[foo s1]))
                      (testing "and the input-key is bound to the correct variable"
                        (is (= i 's2)))
                      (testing "and the start body of the suspending node constitutes the body of the resume expression"
                        (is (= b '[(complex-suspending)])))
                      (testing "and nothing follows the resume expression"
                        (is (empty? remaining-body))))))))
            (testing "where the tail partition"
              (testing "is :valued"
                (is (not (p/suspending? tailp))))
              (testing "contains the expected parameters"
                (is (= '[foo s1 intermed-p1 intermed-p2] (:params tailp))))
              (testing "has the expected body"
                (is (= '[{:bindings [[s2 (complex-tail)] [s3 (valued2)]] :syms [s1]}] (:body tailp)))))))))))