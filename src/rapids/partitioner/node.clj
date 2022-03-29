(ns rapids.partitioner.node
  (:require [rapids.objects.address :refer [address?]]
            [rapids.partitioner.partition :as p]
            [rapids.objects.address :as a]
            [rapids.partitioner.macroexpand :as m]
            [rapids.partitioner.partition-utils :refer [constant? add-params]]
            [rapids.support.util :as util]
            [rapids.partitioner.resume-at :refer [resume-at]])
  (:import (rapids.objects.address Address)
           (com.google.javascript.jscomp.newtypes PersistentMap)))

(use 'debux.core)
;;;; Node

;;; Represents a partitioned expression. The node stores a table of partitions and contains
;;; two pointers, called :start and :value. These are addresses which point to the partitions
;;; used to start computing the value the node represents and the partition which returns
;;; the value of the node. A node representing a regular Clojure expression (i.e., a non-suspending
;;; expression) has the same start and value address.
;;;
;;; A computed pointer called :tail indicates the partition which is available
;;; to be extended by the partitioner. It could be a :valued or :suspending and
;;; could be the :start or :value partition, however, if a :value partition is present
;;; :tail will always point at the :value partition.
;;;
;;; Nodes provide a higher level abstraction required for reconnecting the partitioned
;;; expressions. For example,
;;; (chain n1 n2) ; is used to

;;;; PartitionMap
(defrecord Node
  [^Address start
   ^Address value
   ^PersistentMap partitions])

(defn node? [o] (instance? Node o))

(defn ->node
  ([]
   (->Node nil nil {})))

(defn ->suspending-node [addr params body]
  (->Node addr nil {addr (p/->partition params body :suspending)}))

(defn ->valued-node [addr params body]
  (->Node addr addr {addr (p/->partition params body :valued)}))

(defn addresses [node]
  (-> node :partitions keys))

(def symbolic-address? #{:start :value :tail})

(defn size [node]
  (count (:partitions node)))

(defn true-address
  "Given a symbolic address name (:start, :value or :tail), returns the true address.
  If an address is provided, returns the address. :tail is equivalent to
  `(or (true-address node :value) (true-address node :start))`"
  [node addr]
  {:pre [(node? node) (or (address? addr) (symbolic-address? addr))]}
  (case addr
    :start (:start node)
    :value (:value node)
    :tail (or (:value node) (:start node))
    addr))

(defn get-partition
  "Returns the partition at the actual or symbolic address"
  [node addr]
  (get-in node [:partitions (true-address node addr)]))

;;(defn suspending?
;;  "True if the node is not simple"
;;  [node]
;;  (-> (get-partition node :start) :type (not= :valued)))

(defn add-partition
  "Adds a partition to the node, optionally setting the given address as the start or value address.
  Note that the start address of a node may not be changed once set."
  ([node address p & {:keys [start value]}]
   {:pre [(address? address) (p/partition? p)
          (or (not start) (not (get-partition node :start)))]}
   (cond-> (assoc-in node [:partitions address] p)
     start (assoc :start address)
     value (assoc :value address))))

(defn atomic?
  "A node with a partition which serves both as the start and value."
  [node]
  (when (= (:start node) (:value node))
    (assert (-> (get-partition node :start) :type (= :valued)))
    true))

(defn suspends? [node]
  (not (atomic? node)))

(defn has-tail-value?
  "Returns true if the tail partition is simple, indicating that the value of the
  body may be used directly as an expression."
  [node]
  (let [tail (get-partition node :tail)]
    (assert tail "Invalid attempt to call has-tail-value? on node with no partitions")
    (-> tail :type (= :valued))))

(defn get-partition [node addr]
  (get-in node [:partitions (true-address node addr)]))

(defn update-partition
  "Updates the partition at addr. The function f is passed the partition followed by any args.

  (update-partition node :start myfn 1 2 3) => node with updated partition"
  [node addr f & args]
  {:pre [(fn? f) (node? node)]}
  (apply update-in node [:partitions (true-address node addr)] f args))

(defn make-value-address [node] (a/child (:start node) '_value))
(defn ensure-tail-value
  "Ensures the node has a value partition. input-key may be a symbol, true or nil, indicating
  the symbol the node's value should be bound to. Providing nil means that the value will not be bound.
  By default, binds the "
  ([node] (ensure-tail-value node true))
  ([node input-key]
   {:pre [((some-fn symbol? false? #{true}) input-key)]}
   (let [tail-part (get-partition node :tail)
         tail-type (:type tail-part)]
     (assert (p/partition-type? tail-type))
     (assert (not= tail-type :resumed) "Inconsistent partition - tail partition type should never be :resumed")
     (if (= tail-type :valued) node
       ;; tail-partition is :suspending - we need to resume it at a new value-partition:
       (let [value-addr    (make-value-address node)
             params        (:params tail-part)
             param         (cond
                             (symbol? input-key) input-key
                             input-key (m/stable-symbol))
             new-tail-part (if param (p/->partition (conj params param) [param] :valued)
                             (p/->partition params [] :valued))]
         (-> (update-partition node :tail p/add-resume-at value-addr param)
           (add-partition value-addr new-tail-part :value true)))))))

(defn distinct-addresses?
  "True when the partitions of the given nodes are all at distinct addresses, otherwise false."
  [& nodes]
  (or (-> nodes count (< 2))
    (->> nodes
      (map (comp set addresses))
      (apply clojure.set/intersection)
      empty?)))

(defn combine-partitions
  "Combines the partition maps of two nodes, ensuring the addresses do not overlap"
  [node & nodes]
  {:pre  [(node? node) (every? node? nodes)
          (apply distinct-addresses? node nodes)]
   :post [(node? %)]}
  (apply update node :partitions merge (map :partitions nodes)))

(defn remove-partition
  "Removes the partition at the given address. If the address is references by the :start or :value pointers,
  the pointer is cleared."
  [node addr]
  {:pre [(node? node) (or (symbolic-address? addr) (address? addr))]}
  (let [addr (true-address node addr)]
    (cond-> (update node :partitions dissoc addr)
      (= (:start node) addr) (assoc :start nil)
      (= (:value node) addr) (assoc :value nil))))

(defn chain
  "Returns a new node which represents sequential execution of code in node1 followed by node2.

  This is the conceptual equivalent of `concat`ing two vectors of Clojure expressions."
  [n1 n2]
  {:pre [(= (:params (get-partition n1 :tail)) (:params (get-partition n2 :start)))]}
  (let [n2-start (get-partition n2 :start)]
    (-> (ensure-tail-value n1)
      (combine-partitions n2)
      (update-partition :tail p/extend-body (:type n2-start) (:body n2-start))
      (remove-partition (true-address n2 :start)))))

(declare make-binding-partition node-from-binding-group
  add-binding-group resuming-body-fn make-let-body)

(defn move-partition
  "Moves a partition from one address to another. Exercise with caution - although this function updates :start
  and :value pointers, it does not references to the from-addr in partition code."
  [node from-addr to-addr]
  {:pre [(not= from-addr to-addr)]}
  (cond-> (-> (add-partition node to-addr (get-partition node from-addr))
            (remove-partition from-addr))
    (= from-addr (:start node)) (assoc :start from-addr)
    (= from-addr (:value node)) (assoc :value from-addr)))

(defn binding-group? [o]
  (and (sequential? o)
    (every? (some-fn symbol? nil?) (map first o))
    (every? node? (map second o))
    #_(every? (comp atomic? second) (butlast o))))

(defn extend-body
  "Treats the node as a body (which may suspend), adding another expression to the body"
  [node ptype body]
  {:pre [(p/partition-type? ptype)]}
  (-> (ensure-tail-value node)
    (update-partition :tail p/extend-body ptype (vec body))))

(defn body-value-expr
  "Gets an expression which represents the value returned by the node's value partition. "
  [node]
  (let [p (get-partition node :value)]
    (assert (:type p) :valued)
    (p/body-value-expr p)))

(defn bind
  "Returns a node which executes a body of code with node values bound to the given
  symbols. Calls body-fn which should return a vector of expressions which will be
  executed with the desired bindings. Arguments to body-fn are given below.

  params - the params available to the body being evaluated
  syms - symbols to be bound, the left hand sides of the bindings
  nodes - nodes representing partitioned expressions, the right hand sides of the bindings
  body-fn - fn of two arguments [bound-syms, new-bindings]
     bound-syms - vector of symbols which have been bound in previous partitions, provided as arguments to the last partition
     new-bindings - vector two-tuples [sym expr] representing bindings which have not yet been made
                all of the expressions are guaranteed to be non-suspending.
     returns vector of Clojure expressions
  ptype - the resulting partition type (:valued, :suspending, :resumed) of the body returned by body-fn

  This method is used to generate bindings for both let expressions and function call-style
  expressions. The body-fn argument can decide whether to use binding symbols (as in the case
  of a let form, or to ignore them (as in the case of a function call). This allows recapitulating
  the original code with inline argument evaluation - e.g, for expressions such as:

  `(f (g (<*)))` => `(f (g <<1>>))`
  instead of (let [<<2>> (g <<1>>)] (f <<2>>))"
  [expr-addr params syms nodes body-fn ptype]
  {:pre [(address? expr-addr) (vector? params) (every? simple-symbol? params)
         (= (count syms) (count nodes))
         (every? node? nodes) (every? simple-symbol? syms)
         (fn? body-fn)]}
  (letfn [(add-bindings
            [[node bound-syms] [binding-group node-extender]]
            {:pre [(binding-group? binding-group)]}
            (let [last-node                (some-> binding-group last second)
                  last-suspends?           (and last-node (suspends? last-node))

                  bgroup-minus-start-parts (map #(remove-partition (second %) :start) binding-group)
                  new-node                 (->
                                             ;; add the internal partitions of the given nodes
                                             (apply combine-partitions node bgroup-minus-start-parts)
                                             ;; extend the existing
                                             (node-extender bound-syms binding-group))
                  _                        (assert (:value new-node))
                  non-suspending-bindings  (if last-suspends? (butlast binding-group) binding-group)
                  non-suspending-syms      (map first non-suspending-bindings)
                  bound-syms               (dedupe (concat bound-syms non-suspending-syms))]
              [new-node bound-syms]))
          (make-binding-extender [addr]
            {:pre [(address? addr)]}
            (fn [node bound-syms new-bindings]
              {:post [(node? %)]}
              (let [valued-bindings  (butlast new-bindings)
                    [input-key susp-node] (last new-bindings)
                    _                (assert (suspends? susp-node))
                    let-bindings     (map (fn [[s n]] [s (body-value-expr n)]) valued-bindings)
                    partition-params (distinct (concat params bound-syms))
                    new-params       (vec (distinct (concat partition-params (map first (butlast new-bindings)))))
                    start-body       (:body (get-partition susp-node :start))
                    resume-body      (make-let-body let-bindings
                                       [`(resume-at [~addr ~new-params ~input-key]
                                           ~@start-body)])
                    ;; create an empty tail partition
                    tail-partition   (p/->partition (add-params new-params input-key) [] :valued)]
                ;; add the resume expression, and change the value address appropriately
                (assert addr)
                (-> node
                  (extend-body :resumed resume-body)
                  (add-partition addr tail-partition)
                  (assoc :value addr)))))
          (make-carry-over-binding [bgroup]
            (let [[sym node] (last bgroup)]
              [sym (ensure-tail-value node sym)]))
          (final-extender [node bound-syms binding-group]
            {:pre [(every? simple-symbol? bound-syms)
                   (every? #(and (vector? %) (-> % count (= 2))) binding-group)]}
            (let [bindings (mapv (fn [[sym node]] (vector sym (body-value-expr node))) binding-group)]
              (extend-body node ptype (body-fn bound-syms bindings))))]
    (let [;; associate the symbols with the nodes, splitting at the suspending nodes
          ;;  syms: [_1 _2 _3 _4 _5 _6...], nodes: [a1 a2 S3 a4 S5 a6...], where S3 and S6 are suspending nodes
          ;;  binding-groups =>
          ;;    (([ _1 a1], [_2 a2], [_3 S3]), ; first partition
          ;;     ([_4 a4], [_5 S5]),          ; second partition
          ;;     ([_6 a6...]...))             ; etc.
          ;;
          binding-groups      (util/partition-when (comp suspends? second) (map vector syms nodes))
          ;; ensure there is a final non-suspending binding group:
          binding-groups      (if (or (empty? binding-groups)
                                    (some-> binding-groups last last second suspends?))
                                (concat binding-groups [()])
                                binding-groups)

          ;; duplicate the final suspending node binding of each binding group onto the beginning of the
          ;; next binding group - this is because the suspending node is started in the previous
          ;; binding group and the value of the value partition gets bound in the next partition
          carry-over-bindings (map make-carry-over-binding (butlast binding-groups))

          binding-groups      (cons (first binding-groups)
                                (map (fn [carry-over binding-group] (cons carry-over binding-group))
                                  carry-over-bindings
                                  (concat (rest binding-groups) (repeat []))))

          ;; use the expr-addr for the last address
          next-addresses      (map #(some-> % first second :start) (rest binding-groups)) #_(concat (butlast (map #(some-> % first second :start) (rest binding-groups)))
                                                                                              [expr-addr])
          binding-extenders   (map make-binding-extender next-addresses)

          ;; each extender takes [node bound-syms new-bindings] and extends the node with new bindings
          ;; (in the case of the resuming-extender
          extenders           (concat binding-extenders [final-extender])

          ;; each "binding-tail-group" = [binding-group extender]
          ;; i.e., a binding group and a function for extending the node with those bindings
          _                   (assert (= (count binding-groups) (count extenders)))
          binding-tail-groups (map vector binding-groups extenders)
          initial-node        (->valued-node expr-addr params [])
          bound-syms          []]
      (first (reduce add-bindings [initial-node bound-syms]
               binding-tail-groups)))))

(defn partition-map-def
  "Generates expression of the form `{ <address1.point> (fn [...]...) <address2.point> ...}`"
  [node]
  (let [pfdefs (map-indexed (fn [index [address _]]
                              [`(quote ~(:point address)) (p/partition-fn-def pmap address index)])
                 (node :partitions))]
    (apply hash-map (apply concat pfdefs))))

(defn make-let-body [bindings body]
  {:pre [(sequential? bindings)
         (every? (comp simple-symbol? first) bindings)
         (every? #(-> % count (= 2)) bindings)
         (sequential? body)]}
  (let [let-bindings (vec (apply concat (filter (fn [s v] (not= s v)) bindings)))]
    (if (-> let-bindings count (> 0))
      [`(let [~@let-bindings] ~@body)]
      body)))

;; HELPERS
(defn- capture-input-node-binding
  "Inspects the input-node and binds the input-node to the input-sym when the input-node's
  value expression is not a symbol. For example, in an expression such as
  `(f (g (<*)))`, the node start will be `(<*)` and the node value partition body will be
  something like `[(f (g <<1>>))]`. The input node's start expression was evaluated in the previous
   partition. To continue the computation. `capture-input-node-binding` detects that the value
   is not a symbol and ensures that the value expression is included in the bindings."
  [input-sym input-node let-bindings]
  (or (if input-node
        (let [body-value (p/body-value-expr input-node)]
          (if-not (symbol? body-value)
            (vec (concat [input-sym body-value] let-bindings)))))
    let-bindings))

(defn- node-with-bindings
  "The binding group contains one or more nodes, where the last node is suspending"
  [bound-keys input-node syms nodes next-address]
  (let [binding-partition (make-binding-partition bound-keys input-node syms nodes next-address)
        combined-node     (apply combine-partitions (map #(remove-partition % :start) nodes))]
    (update-partition combined-node :value p/extend-body binding-partition)))

;;(defn- make-binding-partition
;;  "This function given a sequence of key-node pairs, where the last node is suspending,
;;   returns a partition which binds the keys to the node values and resumes at next-address
;;   with the suspending node-value bound to the tail key.
;;
;;  A binding set is a sequence of symbol-node pairs:
;;  ([s1 node1] [s2 node2]... [s3 noden]) where all but the last pair are guaranteed to
;;  contain non-suspending nodes, and the last is guaranteed to be a suspending node.
;;
;;  Returns a partition"
;;  [bound-keys input-node syms nodes body-fn ptype]
;;  {:pre [(every? atomic? (butlast nodes))
;;         (fn? body-fn) (p/partition-type? ptype)
;;         (or (nil? input-node) (node? input-node))
;;         (not (atomic? (last nodes)))]}
;;  (let [first-node    (first nodes)
;;        let-values    (map #(-> (get-partition % :value) p/body-value-expr) (butlast nodes))
;;        let-syms      (butlast syms)
;;        let-bindings  (vec (interleave let-syms let-values))
;;        let-bindings  (capture-input-node-binding (last let-bindings) input-node let-bindings)
;;        final-node    (last nodes)
;;        params        (vec (concat (:params (get-partition first-node :start)) bound-keys))
;;        resume-params (vec (concat params let-syms))
;;        input-key     (last syms)
;;        body          (:body (get-partition final-node :start))]
;;    (->
;;      (apply combine-partitions first-node (map #(remove-partition % :start) (rest nodes)))
;;      (update-partition :value p/extend-body body-fn))
;;    (p/->partition
;;      params
;;      [(make-let-body let-bindings
;;         `(resume-at [~next-address ~resume-params ~input-key] ~@body))]
;;      :resumed)))
#_(defn- make-binding-partition
    "This function given a sequence of key-node pairs, where the last node is suspending,
     returns a partition which binds the keys to the node values and resumes at next-address
     with the suspending node-value bound to the tail key.

    A binding set is a sequence of symbol-node pairs:
    ([s1 node1] [s2 node2]... [s3 noden]) where all but the last pair are guaranteed to
    contain non-suspending nodes, and the last is guaranteed to be a suspending node.

    Returns a partition"
    [bound-keys input-node syms nodes next-address]
    {:pre  [(every? atomic? (butlast nodes))
            (or (nil? input-node) (node? input-node))
            (address? next-address)
            (not (atomic? (last nodes)))]
     :post [(p/partition? %)]}
    (let [first-node    (first nodes)
          let-values    (map #(-> (get-partition % :value) p/body-value-expr) (butlast nodes))
          let-syms      (butlast syms)
          let-bindings  (vec (interleave let-syms let-values))
          let-bindings  (capture-input-node-binding (last let-bindings) input-node let-bindings)
          final-node    (last nodes)
          params        (vec (concat (:params (get-partition first-node :start)) bound-keys))
          resume-params (vec (concat params let-syms))
          input-key     (last syms)
          body          (:body (get-partition final-node :start))]
      (p/->partition
        params
        [(make-let-body let-bindings
           `(resume-at [~next-address ~resume-params ~input-key] ~@body))]
        :resumed)))

;; TODO: Delete

;;(defn using-value
;;  "Returns a new node, where the value of the given node is used in a new body.
;;
;;  body-fn is a function which takes a Clojure expression as an argument and returns
;;  a vector representing a code body.
;;  suspending? indicates whether the body is suspending."
;;  [node suspending? body-fn]
;;  {:pre [(node? node) (fn? body-fn)]}
;;  (let [node     (ensure-bound node)
;;        new-body (body-fn (get-partition node :value))]
;;    (assert (vector? new-body))
;;    (update-partition node :value
;;      p/update-body suspending? (comp body-fn p/body-value-expr))))
