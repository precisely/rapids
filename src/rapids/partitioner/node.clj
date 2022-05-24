(ns rapids.partitioner.node
  (:require [rapids.objects.address :refer [address?]]
            [rapids.partitioner.partition :as p]
            [rapids.objects.address :as a]
            [rapids.partitioner.macroexpand :as m]
            [rapids.partitioner.partition-utils :refer [add-params make-let-body]]
            [rapids.support.util :as util]
            [rapids.partitioner.resume-at :as r]
            [clojure.set :as set]
            [debux.core :as debux]
            [rapids.partitioner.recur :refer [with-tail-position]])
  (:import (rapids.objects.address Address)
           (com.google.javascript.jscomp.newtypes PersistentMap)
           (clojure.lang PersistentVector)))

;;;; Node

;;; Represents a partitioned expression. The node stores a table of partitions and contains
;;; two pointers, called :head and :tail. The head address references the partition
;;; which begins the computation, and the tail address represents the partition where
;;; the partitioner may add new code.
;;;
;;; Partitions themselves may be suspending or not. A non-suspending partition returns
;;; a value, and new forms may be added to its body. A suspending partitions may be resumed
;;; at a new partition.
;;;
;;; A node representing a regular Clojure expression will "atomic-valued",
;;; that is, the :head and :tail addresses will be the same, and the partition
;;; will be non-suspending.
;;; ;;;
;;; Nodes provide a higher level abstraction required for reconnecting the partitioned
;;; expressions. For example,
;;; (chain n1 n2) ; is used to continue the computation of n1 with n2 - in other words,
;;; having the logical effect of concatenating the bodies of the n1 and n2.

(defrecord RecurPoint
  [^Address address
   ^PersistentVector params])

(defrecord Node
  [^Address head
   ^Address tail
   ;;^RecurPoint recur-point
   ;;^PersistentVector bindings
   ^PersistentMap partitions
   ]
  Object
  (toString [o] (pr-str o)))

(defn node? [o] (instance? Node o))
(declare valid-node?)

(defn ->suspending-node [addr params body]
  {:post [(valid-node? %)]}
  (->Node addr addr {addr (p/->partition params body true)}))

(defn ->valued-node [addr params body]
  {:post [(valid-node? %)]}
  (->Node addr addr {addr (p/->partition params body false)}))

(defn addresses [node]
  (-> node :partitions keys))

(def symbolic-address? #{:head :tail})

(defn size [node]
  (count (:partitions node)))

(defn true-address
  "Given a symbolic address name (:head or :tail), returns the true address.
  If an address is provided, returns the address. :tail is equivalent to
  `(or (true-address node :value) (true-address node :head)`"
  [node addr]
  {:pre [(node? node) (or (address? addr) (symbolic-address? addr))]}
  (case addr
    :head (:head node)
    :tail (:tail node)
    addr))

(defn get-partition
  "Returns the partition at the actual or symbolic address"
  ([node] (partial get-partition node))
  ([node addr]
   {:pre [(node? node)]}
   (get-in node [:partitions (true-address node addr)])))

(defn add-partition
  "Adds a partition to the node, optionally setting the given address as the head or tail address.
  Note that the head address of a node may not be changed once set."
  ([node address params body suspending]
   (add-partition node address (p/->partition params body suspending)))
  ([node address p]
   {:pre  [(address? address) (p/partition? p)]
    :post [(valid-node? %)]}
   (assoc-in node [:partitions address] p)))

(defn atomic?
  "A node with a partition which serves both as the head and tail."
  [node]
  (and (node? node)
    (= (:head node) (:value node))))

(defn atomic-valued?
  "An atomic node which may be substituted into another form (as a Clojure expression)"
  [node]
  (and (atomic? node)
    (-> (get-partition node :tail)
      :suspending
      not)))

(defn suspends?
  "True if the given partition of the node suspends. The unary form is equivalent to
  (suspends? node :head), which also indicates whether evaluating the node involves
  any suspending expressions."
  ([node] (suspends? node :head))
  ([node addr]
   (p/suspending? (get-partition node addr))))

(defn has-tail-value?
  "Returns true if the tail partition is simple, indicating that the value of the
  body may be used directly as an expression."
  [node]
  (let [tail (get-partition node :tail)]
    (assert tail "Invalid attempt to call has-tail-value? on node with no partitions")
    (not (p/suspending? tail))))

(defn update-partition
  "Updates the partition at addr. The function f is passed the partition followed by any args.

  (update-partition node :head f) => node with updated partition"
  [node addr f & args]
  {:pre  [(fn? f) (node? node)]
   :post [(valid-node? %)]}
  (apply update-in node [:partitions (true-address node addr)] f args))

(defn ^{:arglists '([node addr suspending f & args]
                    [node addr f & args])
        :doc      "Updates the body of the partition at the given address, optionally
        setting the suspending flag."}
  update-body [node addr & args]
  {:pre [(some fn? ((juxt first second) args))]}
  (apply update-partition node addr p/update-body args))

(defn remove-unreferenced-partitions [node addresses]
  (update node :partitions
    #(apply dissoc %1 %2) (set/difference (set addresses) (set [(:head node) (:tail node)]))))

(defn make-value-address [node] (a/child (:head node) '%value))

(defn resume
  "Returns a Resumes the node `from` the :head or :tail `to` the given address"
  [node from to & {:keys [param body]
                   :or   {body []}}]
  {:pre [(node? node) (symbolic-address? from) (address? to)]}
  (let [from-partition (get-partition node from)
        _              (assert (p/suspending? from-partition))
        params         (vec (concat (-> from-partition :params) (if param [param])))
        new-tail-part  (p/->partition params body false)]
    (->
      ;; resume the from partition at the to address:
      (update-partition node from p/add-resume-at to param)
      ;; make a partition at the new address
      (add-partition to new-tail-part)
      ;; and set it as the new tail
      (assoc :tail to))))

(defn combine-partitions
  "Combines the partition maps of two nodes, ensuring the addresses do not overlap"
  [node & nodes]
  {:pre  [(node? node) (every? node? nodes)]
   :post [(valid-node? %)]}
  (let [pmappings (dedupe (concat (map :partitions (cons node nodes))))
        addresses (map first pmappings)]
    (assert (distinct? addresses)
      (str "Partition address conflict detected while attempting to combine nodes"
        pmappings))
    (assoc node :partitions (into {} pmappings))))

(defn remove-partition
  "Removes the partition at the given address. If the address is referenced by the :head or :tail pointers,
  the pointer is cleared."
  [node addr]
  {:pre  [(node? node) (or (symbolic-address? addr) (address? addr))]
   :post [(valid-node? %)]}
  (let [addr (true-address node addr)]
    (cond-> (update node :partitions dissoc addr)
      (= (:head node) addr) (assoc :head nil)
      (= (:tail node) addr) (assoc :tail nil))))

(defn chain
  "Returns a new node which represents sequential execution of code in node1 followed by node2.

  This is the conceptual equivalent of `concat`ing two vectors of Clojure expressions."
  [n1 n2]
  {:pre  [(= (:params (get-partition n1 :head)) (:params (get-partition n2 :head)))]
   :post [(valid-node? %)]}
  (let [n1-tail       (get-partition n1 :tail)
        n2-head-addr  (:head n2)
        n2-head       (get-partition n2 :head)
        combined-node (combine-partitions n1 n2)
        joined-node   (if (p/suspending? n1-tail)
                        (update-partition combined-node :tail p/add-resume-at n2-head-addr (:params n1))
                        (-> (update-partition combined-node :tail p/extend-body (p/suspending? n2-head) (:body n2-head))
                          (remove-partition n2-head-addr)))]
    (if (get-partition joined-node (:tail n2))
      (assoc joined-node :tail (:tail n2))
      joined-node)))

(declare make-binding-partition node-from-binding-group
  add-binding-group resuming-body-fn)

(defn binding-group? [o]
  (and (sequential? o)
    (every? (some-fn symbol? nil?) (map first o))
    (every? node? (map second o))))

;(defn ensure-tail-extendible
;  "Ensures the node has a non-suspending tail partition.
;
;  input-key may be a symbol, true or nil, indicating the symbol the node's value
;  should be bound to. Providing nil (the default) means that the value will not be bound."
;  [node & {:keys [new-params input-key] :or {new-params []}}]
;  {:pre  [(node? node) (every? simple-symbol? new-params)]
;   :post [(valid-node? %)]}
;  (let [tail-part (get-partition node :tail)]
;    (if (not (p/suspending? tail-part))
;      node
;      ;; tail-partition is suspending - we need to resume it at a new value-partition:
;      (resume node :tail (make-value-address node)
;        :params (apply add-params (:params tail-part) new-params)
;        :input-key input-key))))

(defn ensure-bindable
  "Ensures a node's value can be bound. Specifically, returns a node such that the body-value-expr of the tail partition
   can be substituted into an expression. The tail partition of the returned node is guaranteed to be non-suspending.
   The returned node may be the given node."
  ([node] (ensure-bindable node (m/stable-symbol)))
  ([node suggested-symbol]
   (if (p/suspending? (get-partition node :tail))
     (let [param suggested-symbol
           from  :head
           to    (make-value-address node)]
       (resume node from to :param param :body [param]))
     node)))

(defn body-value-expr
  "Gets an expression which represents the value returned by the node's value partition. "
  [node]
  (let [p (get-partition node :tail)]
    (assert (not (p/suspending? p)))
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
  suspending - true if the partition of the body returned by body-fn is a suspending expression

  This method is used to generate bindings for both let expressions and function call-style
  expressions. The body-fn argument can decide whether to use binding symbols (as in the case
  of a let form, or to ignore them (as in the case of a function call). This allows recapitulating
  the original code with inline argument evaluation - e.g, for expressions such as:

  `(f (g (<*)))` => `(f (g <<1>>))`
  instead of (let [<<2>> (g <<1>>)] (f <<2>>))"
  [addr params syms args partitioning-fn body-fn suspending]
  {:pre  [(address? addr) (vector? params) (every? simple-symbol? params)
          (= (count syms) (count args))
          (every? simple-symbol? syms)
          (fn? partitioning-fn)
          (fn? body-fn)
          (boolean? suspending)]
   :post [(valid-node? %)]}
  (letfn [(add-bindings
            [[node bound-syms] [binding-group node-extender]]
            {:pre [(binding-group? binding-group)]}
            (let [binding-nodes           (map second binding-group)
                  last-node               (last binding-nodes)
                  last-suspends?          (and last-node (suspends? last-node))

                  new-node                (->
                                            ;; add the internal partitions of the given nodes
                                            (apply combine-partitions node binding-nodes)
                                            (remove-unreferenced-partitions (map :head binding-nodes)) ;; extend the existing
                                            (node-extender bound-syms binding-group))
                  non-suspending-bindings (if last-suspends? (butlast binding-group) binding-group)
                  non-suspending-syms     (map first non-suspending-bindings)
                  bound-syms              (dedupe (concat bound-syms non-suspending-syms))]
              [new-node bound-syms]))
          (make-binding-extender [next-addr]
            {:pre [(address? next-addr)]}
            (fn [node bound-syms new-bindings]
              {:post [(node? %)]}

              (let [valued-bindings  (butlast new-bindings)
                    [input-key susp-node] (last new-bindings)
                    _                (assert (suspends? susp-node))
                    let-bindings     (map (fn [[s n]] [s (body-value-expr n)]) valued-bindings)
                    partition-params (distinct (concat params bound-syms))
                    new-params       (vec (distinct (concat partition-params (map first (butlast new-bindings)))))
                    head-body        (:body (get-partition susp-node :head))
                    resume-body      (make-let-body let-bindings
                                       [`(r/resume-at [~next-addr ~new-params ~input-key]
                                           ~@head-body)])
                    ;; create an empty tail partition
                    tail-partition   (p/->partition (add-params new-params input-key) [] false)]
                ;; add the resume expression, and change the tail address appropriately
                (assert next-addr)
                (let [node (-> node
                             (update-body :tail true (constantly resume-body))
                             (add-partition next-addr tail-partition)
                             (assoc :tail next-addr))]
                  node))))
          (make-carry-over-binding [bgroup]
            (let [[sym node] (last bgroup)]
              [sym (ensure-bindable node sym)]))
          (final-extender [node bound-syms binding-group]
            {:pre [(every? simple-symbol? bound-syms)
                   (every? #(and (vector? %) (-> % count (= 2))) binding-group)]}
            (let [bindings (mapv (fn [[sym node]] (vector sym (body-value-expr node))) binding-group)]
              (update-body node :tail suspending (constantly (body-fn bound-syms bindings)))))]
    (let [nodes               (with-tail-position false
                                (map-indexed (fn [idx arg]
                                               (partitioning-fn arg (a/child addr idx)
                                                 (apply add-params params (subvec (vec syms) 0 idx))))
                                  args))
          ;; associate the symbols with the nodes, splitting at the suspending nodes
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

          next-addresses      (map #(some-> % first second :tail) (rest binding-groups))
          binding-extenders   (map make-binding-extender next-addresses)

          ;; each extender takes [node bound-syms new-bindings] and extends the node with new bindings
          ;; (in the case of the resuming-extender
          extenders           (concat binding-extenders [final-extender])

          ;; each "binding-tail-group" = [binding-group extender]
          ;; i.e., a binding group and a function for extending the node with those bindings
          _                   (assert (= (count binding-groups) (count extenders)))
          binding-tail-groups (map vector binding-groups extenders)
          initial-node        (->valued-node addr params [])
          bound-syms          []]
      (first (reduce add-bindings [initial-node bound-syms]
               binding-tail-groups)))))

(defn partition-map-def
  "Generates expression of the form `{ <address1.point> (fn [...]...) <address2.point> ...}`"
  [node]
  (debux.core/dbg
    (let [pfdefs (map-indexed (fn [index [address partition]]
                                [`(quote ~(:point address)) (p/partition-fn-def partition address index)])
                   (:partitions node))]
      (apply hash-map (apply concat pfdefs)))))

(defn- valid-node? [n]
  (and (node? n)
    (-> n :head address?)
    (let [headp (get-partition n :head)]
      (if (atomic-valued? n)
        (not (p/suspending? headp))
        (boolean headp)))))
