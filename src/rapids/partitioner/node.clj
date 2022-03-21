(ns rapids.partitioner.node
  (:require [rapids.partitioner.macroexpand :refer [stable-symbol]]
            [rapids.partitioner.resume-at :refer :all]
            [rapids.partitioner.partition :refer :all]
            [rapids.partitioner.partition-map :as pmap :refer [->pmap ->partition]]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.objects.address :as a]))


;;
;; Nodes are returned by partition-expr functions. A node represents a fragment of the AST,
;; either a regular expression of a suspending expression. Nodes can be combined with
;; other nodes to represent larger expressions.
;;
;; Nodes contain a partition set, mapping from addresses to partitions and two pointers into
;; the partition set, start-address and value-address. The value partition returns the
;; value of the expression the node represents. The start partition begins the
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

  [value                      ; the address of the partition which produces the value of this node
   pmap])                     ; a partition-map

(def ^:const START
  "A special address used to indicate the initial start partition"
  (a/->address 'rapids.partitioner/start))

(defn ->start-node
  "Creates a suspending leaf Node containing a single suspending partition associated with the START address.
  START partition bodies are eventually incorporated into the bodies of other partitions."
  [params start-expr]
  (map->Node {:pmap (pmap/add (->pmap) START params [start-expr] :suspending true)}))

(defn ->value-node
  "Creates a non-suspending leaf Node, containing a single value partition at a specific address."
  [addr params expr]
  (map->Node {:pmap (pmap/add (->pmap) addr params [expr] :suspending false)}))

(defn without-start-partition [node]
  (update node :pmap pmap/delete START))

(defn partition-count
  [node] (-> node :pmap pmap/size))

(defn node-partition-map [n] (-> n :pmap))

(defn get-partition
  "Retrieves the partition at address of the node"
  [node addr]
  (-> node node-partition-map (pmap/get-partition addr)))

(defn value-partition
  "Retrieves the partition which returns the value of the node - nil means no value partition exists (aka the node is unbound)"
  [node]
  (some->> (:value node) (get-partition node)))

(defn start-partition [node]
  (get-partition node START))

(defn is-bound?
  "True if the node has a value partition"
  [node]
  (-> node value-partition boolean))

(defn bind
  "Binds the start partition to the parameter at the given address. This method modifies the
  start partition and adds a value partition "
  ([node addr] (bind node addr (stable-symbol)))
  ([node addr param]
   (if (nil? (start-partition node))
     (throw-partition-error "Attempt to bind node which is missing a start partition"))
   (if (is-bound? node)
     (throw-partition-error "Attempt to bind node which is already bound"))
   (-> node
     (update-in [:pmap START] resuming-at addr param)
     (update :value addr)
     (update :pmap pmap/add addr (vec (conj ~(:params node) param)) param))))

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
