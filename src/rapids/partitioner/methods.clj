(ns rapids.partitioner.methods
  (:require [rapids.objects.address :as a]
            [rapids.objects.closure :refer [->Closure]]
            [rapids.partitioner.node :as n]
            [rapids.objects.flow :as flow]
            [rapids.objects.signals :refer [suspending-operator?]]
            [rapids.partitioner.closure :as closure]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.partitioner.recur :refer [*recur-binding-point* *tail-position* with-binding-point with-tail-position]]
            [rapids.support.debug :refer :all]
            [rapids.support.util :refer :all]
            [rapids.partitioner.resume-at :refer :all]
            [rapids.partitioner.macroexpand
             :refer [partition-macroexpand-1 with-gensym-excluded-symbols stable-symbol]]
            [rapids.partitioner.partition :as p])
  (:import (clojure.lang ArityException LazySeq)))

;;;; Partitioner

;;;
;;; Breaks flow bodies into partitions representing sequences of expressions
;;; which can be executed without being suspending.
;;;
;;; Partitions are the AST representations of code, used to define "partition functions" .
;;; Breaks are introduced by calls to flows or to any suspending function like `input!`.
;;; Suspending functions can be defined by tagging the function ^:suspending. It tells
;;; the partitions to introduce a break because calling the function _might_ return
;;; a suspending event.
;;;
;;; When a flow is called, a StackFrame is created which captures the bindings
;;; and the address where execution should resume. When a `(input! :permit "foo")`
;;; expression is encountered, the stack is persisted to a non-volatile storage
;;; and associated with these values. When an event matching the current `*run-id*`
;;; and `permit` is received, the run (which holds the stack) is retrieved, and
;;; execution is resumed at the point in the flow after the `(input!...)` call,
;;; with the bindings that were present when `(input!...)` was invoked. At the end
;;; of each partition, the frame at the top of the stack is popped and execution
;;; continues at the address in the next frame.
;;;

;;; A small, incomplete example:
;;;
;;; ```
;;; (deflow main [arg]
;;;   (let [result (flow2 arg)]
;;;     (println "(flow2 %s) = %s" arg result)))
;;; ```
;;;
;;; gets broken into 2 partitions, named by addresses (shown in angle brackets).
;;; The partitions contain all the information necessary for generating
;;; partition functions.
;;;
;;; <a1> => ```(fn [{:keys [arg]}] (resume-at [<a2> {:arg arg} 'result] (flow2 arg)))```
;;; <a2> => ```(fn [{:keys [arg result]}] (println "(flow2 %s) = %s" arg result))```
;;;
;;; The addresses represented by <p1> and <p2> are records that describe a unique point
;;; in the flow.
;;;
;;; E.g., <a1> is #rapids.objects.address.Address[main []], representing the beginning of
;;; the main flow.
;;;

(declare partition-body partition-expr partition-fncall-expr
         partition-list-expr partition-flow-invokation-expr partition-flow-body
         partition-functional-expr partition-lexical-bindings
         partition-if-expr partition-let*-expr partition-fn*-expr
         partition-do-expr partition-loop*-expr partition-special-expr partition-recur-expr
         partition-vector-expr partition-map-expr partition-set-expr
         partition-java-interop-expr partition-java-new-expr
         partition-suspend-expr partition-flow-expr partition-callcc-expr
         partition-case-expr partition-case*-expr partition-binding-expr partition-set!-expr)

(defn partition-body [body address params]
  (reduce n/chain (n/->valued-node address params [])
          (map-indexed (fn [idx expr] (partition-expr expr (a/child address idx) params))
                       body)))

;;
;; Expressions
;;
(def non-partitioning? (some-fn constant? symbol? nil? fn?))

(defn partition-expr
  "Breaks an expression down into partitions - blocks of code which are wrapped into
  functions which perform incremental pieces of evaluation.

  Args:
    expr - the expression to be partitioned
    partition = Address of the partition currently being built
    address - Address of the expression
    params - vector of currently bound parameters at this address point
            these become keys when defining functions

  Returns:
  [
    start       - an s-expression which starts computation of expr
    pmap        - a map of addresses to function definitions
    suspend?    - boolean indicating whether this expression could suspend the flow
  ]"
  [expr address params]
  {:pre [(vector? params) (not (some constant? params))]}
  (letfn [(attempt-partitioning []
            (cond
              (non-partitioning? expr) (n/->valued-node address params [expr])
              (vector? expr) (partition-vector-expr expr address params)
              (map? expr) (partition-map-expr expr address params)
              (set? expr) (partition-set-expr expr address params)
              (seq? expr) (partition-list-expr expr address params)))]
    (binding [*partitioning-expr* (if (-> expr meta :line) expr *partitioning-expr*)]
      (or (attempt-partitioning)
          (let [expanded (partition-macroexpand-1 expr)
                expanded (if (and (seq? expanded) (instance? LazySeq expanded))
                           (seq expanded)
                           expanded)]
            (cond
              (non-partitioning? expanded) (n/->valued-node address params [expr])

              (identical? expanded expr)
              (throw-partition-error "Unhandled expression: %s" expr)
              :otherwise (let [m (meta expr)
                               expanded-meta (update (or m {}) :source #(or (:source %) expr))
                               expanded-expr-with-meta (with-meta expanded expanded-meta)]
                           (partition-expr expanded-expr-with-meta address params))))))))

(defn special-form-op? [x] (#{'if 'do 'let* 'fn* 'loop* 'quote 'recur 'case* 'set!} x))
(defn flow-op? [x] (#{'flow 'rapids/flow 'rapids.language.flow/flow} x))
(defn callcc-op? [x] (#{'callcc 'rapids/callcc 'rapids.runtime.cc/callcc} x))
(defn case-op? [x] (#{'case `clojure.core/case} x))
(defn java-inter-op? [x] (= '. x))
(defn fn-op? [x]
  (and (refers-to? (some-fn ifn? fn?) x)
       (if (symbol? x)
         (-> x resolve meta :macro (not= true))
         true)))
(defn fn-like? [x] (#{'throw} x))
(defn java-new-op? [x] (= 'new x))
(defn binding-op? [x] (#{'binding `clojure.core/binding} x))

(defn partition-list-expr
  [expr address params]
  {:pre [(vector? params)]}
  (let [op (first expr)]
    (cond
      ;; attempt to detect operator in expression
      ;; special handling for case expr.
      (case-op? op) (partition-case-expr expr address params)
      (java-inter-op? op) (partition-java-interop-expr expr address params)
      (java-new-op? op) (partition-java-new-expr expr address params)
      (special-form-op? op) (partition-special-expr expr address params)
      (binding-op? op) (partition-binding-expr expr address params)

      ;; note: the following are not special-symbols, though the docs list them as special forms:
      (callcc-op? op) (partition-callcc-expr expr address params)
      (suspending-operator? op) (partition-suspend-expr expr address params)
      (flow/flow-symbol? op) (partition-flow-invokation-expr expr address params)
      (flow-op? op) (partition-flow-expr expr address params)
      (or (fn-like? op) (fn-op? op)) (partition-fncall-expr expr address params))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [[op :as expr] address params]
  (case op
    if (partition-if-expr expr address params)
    do (partition-do-expr expr address params)
    let* (partition-let*-expr expr address params)
    fn* (partition-fn*-expr expr address params)
    loop* (partition-loop*-expr expr address params)
    case (partition-case-expr expr address params)
    case* (partition-case*-expr expr address params)
    quote [expr, nil, false]
    set! (partition-set!-expr expr address params)
    recur (partition-recur-expr expr address params)
    (throw-partition-error "Special operator not yet available in flows %s" op)))

(defn partition-fn*-expr
  "Throws if fn* contain suspending ops - this partitioner acts as a guard
  against knowing or inadvertent inclusion of a suspending operation inside a fn.
  This might happen inadvertently if the user uses a macro (like for) which
  expands into a (fn ...) expression. This is a bit of a blunt instrument,
  but not sure what else to do right now."
  [expr address params]
  (letfn [(check-non-suspending [sig]
            (let [body (rest sig)
                  node (partition-body body address params)]
              (if (n/suspends? node)
                (throw-partition-error "Illegal attempt to suspend in function body"))))]
    (let [[_, sigs] (closure/extract-fn-defs expr)]
      (doseq [sig sigs] (check-non-suspending sig))
      (closure/closure-node expr address params))))

(defn partition-flow-expr
  "Handles anonymous flow definition such as (flow [...] ...).
  Akin to Clojure's (fn [...] ...), but the body may contain suspending expressions.
  The partitioner partitions the flow body and returns a closure which invokes an
  entry point function for the flow."
  [expr, address params]
  (let [entry-address (a/child address '_flow)
        m (meta expr)
        flow-node (partition-flow-body m entry-address (rest expr) params)
        start-expr (with-meta `(->Closure ~entry-address ~(bindings-expr-from-params params) true) m)]
    (-> (n/->valued-node address params [start-expr])
        (n/combine-partitions flow-node))))

(defn partition-java-interop-expr [expr address params]
  (let [source (-> expr meta :source)
        [op & args] source]
    (if source
      (partition-functional-expr op args (meta expr) address params
                                 #(cons op (seq %))
                                 false)
      (throw-partition-error "Java interop operator not yet supported"))))

(defn partition-java-new-expr [expr address params]
  (let [[_ cls & args] expr
        op (symbol (str cls "."))]
    (partition-functional-expr op args (meta expr) address params #(list* 'new %) false)))

(defn partition-let*-expr
  [expr address params]
  (let [binding-address (a/child address 'let-binding)
        body-address (a/child address 'let-body)
        [_ bindings & body] expr
        [keys, args] (map vec (reverse-interleave bindings 2))
        body-node (partition-body body body-address (apply add-params params keys))]
    (partition-lexical-bindings keys args binding-address params body-node)))

;; Dynamic binding: (binding [*foo* "bar"] ...)
;;
;; To ensure dynamic binding semantics
;;   - maintain a FILO stack of dynamic bindings in the Run, the "run dynamic bindings" (RDBs)
;;   - separately manage the RDBs from the thread dynamic bindings (TDBs)
;;   - the (binding [] ...) form is partitioned and has specific behaviors in each partition of the enclosed body:
;;        1. in the first partition:
;;              a the RDBs are saved (pushed) as a hashmap of vars to values
;;              b the entire partition is wrapped by a standard Clojure (binding...) block, i.e.,
;;                     TDBs are pushed at the beginning and popped at the end. This ensures dynamic value
;;                     semantics during the first partition
;;        2 the runtime environment generates TDBs from RDBs at the start of a runlet - e.g., within a
;;              start! or continue! call. I.e., it effectively wraps runlets with a standard Clojure
;;              (binding ...) form, ensuring that any RDBs in the run are made available in the runlet's
;;              dynamic environment
;;        3. code is added to the end of the last partition that pops the RDB hash-map pushed in step 1a
;;
(defn partition-binding-expr
  "Partitions an expression of the form (binding [...] ...). Strategy is to partition the let bindings as normal,
  then generate bindings using the keys. It effectively transforms

  (binding [*a* (foo) *b* (bar)]
    ... body)

  to something like ...

  (let [<<1>> (foo), <<2>> (bar)] ; gensymed symbols used to partition the values
    (push-dynamic-bindings! {#'*a* <<1>>, #'*b* <<2>>})
    (push-thread-dynamic-bindings {#'*a* <<1>>, #'*b* <<2>>})
    (try ...body
       (finally (pop-thread-dynamic-bindings))
    (pop-dynamic-bindings!))

    But we can't just simply rewrite the input then partition it because (try ..) cannot span a body
    with suspending expressions. Hence, the code here. make-dynamic-binding-body-modifier does the bulk
    of the work."
  [expr, address, params]
  (let [address (a/child address 'binding)
        binding-address (a/child address 'bindings)
        body-address (a/child address 'body)
        [_ bindings & body] expr
        [dynvars, args] (map vec (reverse-interleave bindings 2))
        keys (repeatedly (count dynvars) stable-symbol)
        body-bindings (map #(vector %1 %2) dynvars keys)
        bindings-map (into {} (map (fn [[lhs rhs]] `[(var ~lhs) ~rhs]) body-bindings))

        ;; if an error is thrown, the run-bindings do not get stored
        body `[(rapids.runtime.runlet/push-run-bindings! ~bindings-map)
               (let [result# (do ~@body)]
                 (rapids.runtime.runlet/pop-run-bindings!)
                 result#)]
        body-node (-> (partition-body body, body-address, (vec (concat params keys)))
                      ;; in the start partition: we need to establish the thread bindings.
                      ;; subsequent partitions: run dynamic bindings have been established, so the runtime
                      ;; creates thread bindings automatically. All we need to do is undo the RDBs in
                      ;; the final partition.
                      (n/update-partition :head p/update-body
                                          (fn [body]
                                            (if (empty? bindings-map) body
                                                                      `[(rapids.runtime.runlet/enter-binding-body (fn [] ~@body), ~bindings-map, false)]))))]

    (partition-lexical-bindings keys, args, binding-address, params, body-node)))

(defn partition-set!-expr [expr address params]
  (let [[op lhs rhs & disallowed] expr
        isvar? (fn [o] (and (symbol? o) (var? (resolve o))))]
    (if-not (empty? disallowed) (throw (ArityException. 2 'set!)))
    (partition-functional-expr op `(:lhs ~rhs) (meta expr) address params
                               (fn [[_ rhs-param]]
                                 `(do
                                    ~@(if (isvar? lhs)
                                        `((rapids.runtime.runlet/set-run-dynamic-var (var ~lhs) ~rhs-param)))
                                    (set! ~lhs ~rhs-param)))
                               false)))

(defn partition-if-expr
  [expr address params]
  (let [[_ test then else] expr
        address (a/child address 'if)
        [then-addr, else-addr] (map #(a/child address %) ['then 'else])

        then-node (partition-expr then, then-addr, params)

        else-node (partition-expr else, else-addr, params)

        then-sbody (p/body-expr (n/get-partition then-node :head))

        else-sbody (p/body-expr (n/get-partition else-node :head))

        test-node (partition-functional-expr 'if [test] (meta expr) address params
                                             (fn [args] `(if ~@args ~then-sbody ~else-sbody))
                                             (boolean (filter n/suspends? [then-node else-node])))]
    (n/combine-partitions test-node then-node else-node)))

(defn partition-do-expr
  [[op & body] address params]
  {:pre [(= op 'do)]}
  (partition-body body (a/child address 'do) params))

(defn partition-loop*-expr
  "Partitions a (loop ...) expression, establishing a binding-point for recur. If
  this expression suspends, the start body will be a let construct for establishing
  the initial parameters which calls a partition with a loop, which acts as the binding
  point for continuing the loop. If there are no suspending expressiongs, the expression
  is returned.

  Returns a node"
  ;; Note: this function doesn't take a partition-address argument because all operations
  ;; must happen in a new partition defined by the loop, and recur is not allowed in any
  ;; part of the loop definition except the body, where a new partition-address will be created.
  ;; Remember, the partition-address is only used for recur.
  ;;
  ;; In fact, there are 3 partitions of interest to us (assuming the loop is suspending):
  ;; form-partition/0 = where initial values of loop vars are computed and bound
  ;;                    note that because the loop may appear *after*
  ;; form-partition/1 = lexical loop - takes initialized and implements a partial loop
  ;;                    this loop exists to contain recur calls which happen before
  ;;                    any suspensions; i.e., a normal Clojure loop; these recur
  ;;                    calls behave normally
  ;; form-partition/n = any partitions following suspensions in loop-partition/1
  ;;                    recur expressions in these partitions are wrappers
  ;;                    around (flow/exec <loop-partition/1> ...) where
  ;;                    the bindings contain the updated loop variables
  [expr address params]
  (let [op (first expr)
        [_ bindings & loop-body] (if (in? '[loop loop*] op) expr)
        binding-addr (a/child address 'loop-init)
        link-addr (a/child address 'loop-link)
        body-addr (a/child address 'loop-body)

        [loop-params, loop-initializers] (reverse-interleave bindings 2)]

    ;; we need to partition the loop body FIRST, then pass the start-body to partition-bindings
    (let [loop-body-params (apply add-params params loop-params)

          ;; partition the loop body with the loop-partition registered as a recur
          ;; binding point; if recur calls happen within this partition, they
          ;; are treated as regular recur calls; otherwise, the recur is being executed
          ;; in a resumed runlet, so a call to the loop-partition is generated.
          body-node (-> (with-tail-position :reset
                          (with-binding-point [body-addr loop-params loop-body-params] ; partition-recur-expr will use this
                                              (partition-body loop-body body-addr loop-body-params)))

                        ;; place a loop form around the body. The role of this loop is to
                        ;; note: we need a normal (loop ...) expression because we may have normal non-suspending
                        ;; recur expressions in addition to suspending recur expressions.

                        (n/update-partition :head
                                            (fn [p]
                                              (-> p
                                                  (assoc :params loop-body-params)
                                                  (p/update-body
                                                    (fn [body]
                                                      [`(loop [~@(apply concat (map #(vector % %) loop-params))] ~@body)]))))))


          ;; the loop binding node will continues into the loop-node (which we just created)
          ;; we wish to avoid using a stack push to connect them, and instead arrange to directly
          ;; call the partition, and use this as the body of the
          loop-partition-bindings (bindings-expr-from-params loop-body-params)
          linked-body-node (n/combine-partitions
                             (n/->valued-node link-addr
                                              loop-body-params `[(flow/call-partition ~body-addr ~loop-partition-bindings)])
                             body-node)]

      ;; now we can compute the initializers and give it the partitioned loop start...
      (partition-lexical-bindings loop-params, loop-initializers, binding-addr, params, linked-body-node))))

(defn partition-recur-expr
  "Returns: node"
  [expr, address, params]
  (letfn [(make-call [args]
            (let [{partition-params :partition-params,
                   loop-params      :loop-params,
                   loop-address     :address} *recur-binding-point*
                  recur-arity (count loop-params)
                  bindings `(merge
                              ~(bindings-expr-from-params partition-params) ;; ensure all the partition params are represented
                              ~(bindings-expr-from-params loop-params args))]
              (if-not (= (count args) recur-arity)
                (throw-partition-error "Mismatched argument count to recur. Expected: %s args, got: %s"
                                       (count args) recur-arity))
              `(if (= rapids.runtime.globals/*current-partition-address* ~loop-address)
                 (recur ~@args)
                 (flow/call-partition ~(:address *recur-binding-point*) ~bindings))))]
    (if-not *recur-binding-point*
      (throw-partition-error "No binding point"))
    (if-not *tail-position*
      (throw-partition-error "Can only recur from tail position"))

    (let [[op & args] expr]
      (partition-functional-expr op args (meta expr) address params make-call, false))))

(defn bind [address params psyms args body-fn suspending]
  {:pre [(a/address? address)
         (sequential? params) (sequential? psyms) (sequential? args)
         (fn? body-fn)
         (boolean? suspending)]}
  (doall (n/bind address params psyms args partition-expr body-fn suspending)))
;;
;; Partitioning expressions which use a sequence of arguments, any of which could be
;; suspendings
;;
(defn partition-functional-expr [op args m address params expr-fn suspending]
  {:pre [(nilable map? m) (sequential? params) (a/address? address) (fn? expr-fn) (boolean? suspending)]}
  (let [address (a/child address op)
        syms (repeatedly (count args) stable-symbol)

        ;; get a list of constant bindings and partitionable bindings
        [const-bindings p-bindings] (segregate 2 (complement nil?)
                                               (map (fn [s a] (if (constant-expr? a) [[s a] nil] [nil [s a]]))
                                                    syms args))
        [psyms pargs] (segregate 2 (constantly true) p-bindings)
        psyms (vec psyms)

        const-bindings-lookup (into {} const-bindings)
        body-fn (fn [bound-syms bindings]
                  (let [bound-syms (set bound-syms)
                        bindings (into {} bindings)
                        args (map #(cond
                                     (bound-syms %) %
                                     (contains? const-bindings-lookup %) (const-bindings-lookup %)
                                     (contains? bindings %) (get bindings %)
                                     :else (assert false (str "Failed to find binding for " %)))
                                  syms)]
                    [(with-meta (expr-fn args) m)]))]
    (bind address params psyms pargs body-fn suspending)))

(defn partition-lexical-bindings
  "Partitions the bindings, and executes the `body-node` in that context.

  Args:
  keys - sequence of symbols
  args - sequence of expressions to which symbols will be bound
  body-node - a node consisting of a body to be executed with the keys bound to the args

  Returns:
    node which computes the bindings and executes the body-node.
    (note: all the body-node partitions except the start partition are included)"
  [keys, args, address, params, body-node]
  {:pre [(every? simple-symbol? keys)
         (= (count keys) (count args))
         (a/address? address)
         (vector? params)
         (n/node? body-node)]}
  (let [body-startp (n/get-partition body-node :head)]
    (-> (bind address params keys args
              (fn [_ bindings]
                (make-let-body bindings (:body body-startp)))
              (p/suspending? body-startp))
        ;; remove body start partition since it is already included
        (n/combine-partitions body-node)
        (n/remove-partition (:head body-node))
        (assoc :tail (:tail body-node)))))

(defn partition-suspend-expr
  [expr address params]
  (let [[op & args] expr]
    (partition-functional-expr op args (meta expr) address params
                               #(cons op %) true)))

(defn partition-map-expr
  [expr address params]
  {:pre [(map? expr)
         (a/address? address)
         (vector? params)]}
  (let [args (apply concat (seq expr))
        m (meta (or expr (first args)))]
    (partition-functional-expr (symbol "#map") args m address params
                               (fn [args] (into {} (map vec (partition 2 args)))) false)))

(defn partition-vector-expr [expr address params]
  {:pre [(vector? expr)]}
  (partition-functional-expr (symbol "#vec") expr (-> expr first meta) address params vec false))

(defn partition-set-expr [expr address params]
  {:pre [(set? expr)]}
  (partition-functional-expr (symbol "#set") (seq expr) (-> expr first meta) address params set false))

;; Clojure case* is optimized for constant time look up, but it's a pain to
;; partition. My strategy is to just convert to a cond expression. This can't
;; be done with case*, so we just have to throw an error in the unlikely case
;; that a user actually writes that by hand.
(defn partition-case-expr [expr, address, params]
  (let [[_ e & clauses] expr
        [clauses, default] (if (odd? (count clauses))
                             [(butlast clauses), (last clauses)]
                             [clauses, nil])
        evar (gensym)
        cond-clauses (apply concat (map (fn [[left right]] [`(= ~evar ~left) right]) (partition 2 clauses)))
        let-expr (with-meta `(let [~evar ~e]
                               (cond ~@cond-clauses ~@(if default `(:otherwise ~default))))
                            (meta expr))]
    (partition-expr let-expr, address, params)))

(defn partition-case*-expr [expr & _]
  (throw-partition-error "Unable to partition case* expression. Please use case or cond instead" expr))

(defn partition-callcc-expr
  "Partitions an expression of the form `(callcc f)` where `f` is a flow which takes the
  current continuation `cc` as an argument. The current continuation is a function of one
  argument which restarts computation at the current point in the code, returning the value
  passed to `cc`."
  [exp, address, params]
  (let [[op & args] exp
        _ (assert (callcc-op? op))
        [f & _] args
        f (or f `clojure.core/identity)
        faddr (a/child address 'callcc 0)
        fnode (partition-expr f, faddr, params)
        _stack (stable-symbol)
        _dynamics (stable-symbol)
        _fstart (stable-symbol)
        _continuation (stable-symbol)]
    (if (-> args count (> 1))
      (throw (ArityException. (count args) "rapids/callcc")))
    (-> (n/->suspending-node address params
                             [`(let [~_stack (rapids.runtime.runlet/current-run :stack)
                                     ~_dynamics (rapids.runtime.runlet/current-run :dynamics)
                                     ~_fstart ~(n/body-value-expr fnode)
                                     ~_continuation (rapids/fcall rapids.runtime.cc/make-current-continuation ~_stack ~_dynamics)]
                                 (rapids/fcall ~_fstart ~_continuation))])
        (n/combine-partitions fnode))))

(defn partition-flow-invokation-expr
  [expr address, params]
  (let [[op & args] expr]
    (partition-functional-expr op args (meta expr) address params
                               (fn [args]
                                 `(rapids/fcall ~op ~@args))
                               true)))

(defn partition-fncall-expr
  [expr address params]
  {:pre [(vector? params) (a/address? address)]}
  (let [[op & args] expr]
    (partition-functional-expr op args (meta expr) address params
                               (fn [args] `(~op ~@args))
                               false)))

;;;
;;; Partition flow body
;;;

(declare extract-signatures partition-signature)

(defn partition-flow-body
  "Arguments:
  m - meta data, containing the :line of the flow definition
  address - starting address of the flow
  fdecl - the flow declaration, of the form:
           (([arglist1...] body1...) ([arglist2...] body2...)...)
           or
           ([argslist...] body...)
  params - optional vector of params bound in the lexical environment
           (used when generating flow closures)
  Returns:
  [entry-fn-def, node]

  entry-fn-def - a list of the form (fn ([...] ...) ([...] ..) ...)"
  ([m address fdecl]
   (partition-flow-body m address fdecl []))

  ([m address fdecl params]
   (let [sigs (extract-signatures fdecl)
         entry-point-name (str (a/to-string address) "__entry-point")
         [nodes, sig-defs] (reverse-interleave
                             (apply concat
                                    (map-indexed
                                      (fn [idx sig]
                                        (partition-signature (a/child address idx) sig params))
                                      sigs))
                             2)
         combined-node (apply n/combine-partitions nodes)
         entry-fn-def `(fn ~(symbol entry-point-name) ~@sig-defs)]
     [entry-fn-def, combined-node])))

(defn partition-signature
  "Returns a node and a partitioned sig definition.

  For example, this function definition has 2 signatures aka
  E.g., (defn foo
          ([] (foo :bar))
          ([msg] (println \"foo\" msg))"
  [address sig params]
  #d/dbg (let [[args & code] sig
               params (vec (apply add-params (params-from-args args []) params))
               node (with-gensym-excluded-symbols params
                                                  (partition-body (vec code) address params))
               entry-bindings (bindings-expr-from-params params)]
           [node, `([~@args] (flow/call-partition ~address ~entry-bindings))]))

;;
;; HELPERS
;;
(defn extract-signatures [fdecl]
  (let [[name? & sigs] fdecl
        sigs (if (symbol? name?) sigs (cons name? sigs))]
    (cond
      (vector? (first sigs)) (list sigs)
      (list? sigs) sigs
      :else (assert false (str "Invalid flow signature" fdecl)))))

;(defn- params-from-args
;  "given an argument vector, returns a vector of symbols,
;
;  E.g., (params-from-args '[a b & {:keys [c d]}]) => (a b c d)"
;  [args params line]
;  (let [arg (first args)]
;    (cond
;      (= arg '&) (recur (rest args) params line)
;      (map? arg) (recur (rest args) (apply add-params params (:keys arg)) line)
;      (symbol? arg) (recur (rest args) (add-params params arg) line)
;      (nil? arg) (vec params)
;      :else (throw (ex-info (str "Unexpected argument " arg)
;                            {:type :compiler-error})))))
;
;(defn- extract-signatures [fdecl]
;  (let [[_ _ & sigs] (macroexpand `(fn random-name# ~@fdecl))]
;    sigs))
;
