(ns rapids.partitioner.node
  (:require [rapids.partitioner.macroexpand :refer [stable-symbol]]
            [rapids.partitioner.resume-at :refer :all]
            [rapids.partitioner.partition-set :as pset :refer [->pset ->partition]]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.objects.address :as a]))


;;
;; Nodes are returned by partitioner functions. A node represents a fragment of the AST,
;; either a regular expression of a suspending expression. Nodes can be combined with
;; other nodes to represent larger expressions. Simple nodes are just Clojure expressions,
;; and can be combined simply by substituting the expression into the place where
;; the value is needed. E.g., if `? = (bar 123)` and we need `(foo ?)`, we get `(foo (bar 123))`.
;; However, suspending nodes split the computation of a value into potentially multiple
;; steps, involving one or more continue! inputs.
;;
;; Nodes contain a body of code to be executed, a vector of lexically bound parameters,
;; optionally a partition set (a map from addresses to partitions) and an address which
;; identifies which partition
;;

(defrecord Node
  ;; For example, if `myflow` is a flow, then `(myflow)` is the start for a new partition.
  ;;
  ;; A suspending node (node1) could be constructed using `(->suspending-node start params)`
  ;; (def node1 (->suspending-node `(myflow) [])) ; where params indicate which a vector of symbols
  ;;                                          ; which will be lexically bound when (myflow) is called
  ;;
  ;; Then, if `myfn` is an ordinary function,
  ;; `(myfn (myflow) 1 2)` can be constructed
  ;;   (node-with-fn-call node1
  ;;    start = (resume-at [:p1 [] <<1>>] (myflow))
  ;;    value-address = :p1
  ;;    partitions = {:p1 (->partition [<<1>>] [(myfn <<1>>)])}
  ;;
  ;; If we have the expression `(myfn2 (myfn (myflow) 1 2))`,
  ;; node-with-fn-call allows building a new node:
  ;;    start = (resume-at [#a"foo:0" [] <<1>>] (myflow))
  ;;    value-address = #a"foo:0"
  ;;    partitions = {#a"foo:0" (->partition [<<1>>] [(myfn2 (myfn <<1>>))])}

  [start                      ; the starting expression
   params                     ; params available to the start expression
   value-address              ; the address of the partition which produces the value of this node
   partition-set])            ; a partition-set

(defn ->node
  "Creates a Node, which is returned by the partitioner."
  ([start params]
   (->node start params nil nil))
  ([start params addr]
   ;; create a suspending node where the result is delivered to the partition at addr
   (->node start params addr (stable-symbol)))
  ([start params addr binding-symbol]
   {:pre [(sequential? params)
          (not (constant? start))
          (or
            (and (nil? addr) (nil? binding-symbol))
            (and (a/address? addr)
              (simple-symbol? binding-symbol)))]}
   (let [params (vec params)]
     (->Node `(resume-at [~addr ~params ~binding-symbol] ~start) params
       addr (pset/add (->pset) addr
              (conj params binding-symbol)
              [binding-symbol])))))

(defn partition-count
  [node] (-> node :partition-set pset/size))

(defn node-partition-set [n] (-> n :partition-set))

(defn get-partition
  "Retrieves the partition at address of the node"
  [node addr]
  (-> node node-partition-set (pset/get-partition addr)))

(defn value-partition
  "Retrieves the partition which returns the value of the node - nil means no value partition exists (aka the node is unbound)"
  [node]
  (some->> (:value-address node) (get-partition node)))

(defn is-bound?
  "True if the node has a value, and "
  [node]
  (-> node value-partition boolean))

(defn bind-param
  "Binds the result of the suspending expression to the parameter at the given address"
  ([node addr] (bind-param node addr (stable-symbol)))
  ([node addr param]
   (if (or (-> node :value-address nil? not)
         (-> node :start resume-at-form?))
     (throw-partition-error "Suspending node start value already bound"))
   (-> node
     (update :start (fn [start] `(resume-at [~addr ~(:params node) ~param] ~start)))
     (update :value-address addr)
     (update :partition-set pset/add addr (conj ~(:params node) param true)))))

(defn update-value-partition
  "Updates the value partition, using the current value within a new expression.
  The expr-fn takes an expression which returns the current value, and returns a new expression.
  A new partition is returned where the body is substituted with the result of applying expression-fn to
  the current body.

  (update-value-partition {:value-address #a\"foo/bar:0\" "
  [node expr-fn]
  (if-let [vp (value-partition node)]
    ;; value exists...
    (let [body (:body value-partition)]
      (assert (not (filter-tree #{'rapids.partitioner.resume-at/resume-at} body))
        "Attempt to update node value containing resume-at")
      (-> vp
        (update :body (fn [body] `[~@(butlast body) ~(expr-fn (last body))]))))

    ;;
    (throw-partition-error "Attempt to update value of unbound node")))

(defn bind-node-value
  ""
  [node node2]
  )