(ns longterm.partition
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]]
            [longterm.flow :as flow]
            [longterm.util :refer [dissoc-in]]
            [longterm.continuation-set :as cset])
  (:use longterm.stack))

(declare partition-body partition-expr partition-fncall-expr
  partition-list-expr partition-flow-expr
  partition-functional-expr partition-bindings
  partition-if-expr partition-let-expr partition-fn-expr
  partition-do-expr partition-loop-expr partition-special-expr partition-recur-expr
  partition-vector-expr partition-map-expr macroexpand-keeping-metadata
  bindings-expr-from-params with-tail-position with-binding-point
  nsymbols)

(def ^:dynamic *recur-binding-point* nil)
(def ^:dynamic *tail-position*
  "Can be nil, false or true; nil = undefined; false = non-tail; true=tail"
  nil)
;;
;; For partitioning loop/recur
;;

(defmacro with-tail-position [[state] & body]
  `(let [state# ~state]
     (binding [*tail-position* (cond
                                 (false? state#) false
                                 (nil? *tail-position*) state#
                                 :else (and state# *tail-position*))]
       ~@body)))

(defmacro with-binding-point
  [[address params] & body]
  `(binding [*recur-binding-point* {:address ~address :params ~params}]
     ~@body))
;;;; Partitioner

;;;
;;; Breaks flow bodies into blocks which are turned into continuations.
;;; Breaks are introduced by calls to flows or to the `wait-for` function,
;;; which may only appear inside flow bodies.
;;;
;;; When a flow is called, a thunk is created which captures the bindings
;;; and the address where execution should resume. When a `(wait-for event-id)`
;;; expression is encountered, the stack of thunks is persisted
;;; to a non-volatile storage and associated with these values. When an event
;;; matching the current `*flow-id*` and `event-id` is received, the thunk stack is
;;; retrieved, and execution is resumed at the point in the flow after the
;;; `(wait-for)` call, with the bindings that were present when `(wait-for...)`
;;; was invoked. At the end of each continuation, the thunk at the top of
;;; the stack is popped and execution continues at the address in the next
;;; thunk.
;;;

;;; A small, incomplete example:
;;;
;;; ```(deflow flow1 [] (let [foo (flow2)] (print foo)))```
;;;
;;; gets broken into 2 continuations:
;;;
;;; 1: ```(fn [& {:keys []}] (with-thunk [~next-id bindings ~result-key] (flow2)))```
;;; 2: ```(fn [& {:keys [foo]}] (print foo))```
;;;   Where result-key is bound to symbol `foo` and next-id is the continuation-id of
;;;   the second continuation.

(defn partition-body
  [body partition address params]
  "Partitions a list of expressions, e.g., for do, let and deflow forms
  Args:
   body     - list of expressions which should be partitiond
   partition - the current external partition address
   address  - address of the current body; body forms will be address/0, address/1 etc
   params   - vector of symbols which will be bound on entry

  Returns [start-body, cset, suspend?]"
  (let [start-address (address/child address 0)]
    (loop [iter-body    body
           cset         nil
           cur-address  start-address
           partition-address partition
           part-body    []
           start-body   nil
           any-suspend? false]
      (if (> (count iter-body) 0)
        (let [[expr & rest-body] iter-body
              [pexpr expr-cset suspend?] (partition-expr expr partition-address cur-address params)
              cset         (cset/combine cset expr-cset)
              next-address (address/increment cur-address)]
          (if suspend?
            (let [final-part-expr (if (> (count rest-body) 0) `(resume-at [~next-address ~params] ~pexpr) pexpr)
                  part-body       (conj part-body final-part-expr)
                  cset            (cset/add cset partition-address params part-body)]

              (recur rest-body, cset, next-address, next-address, [], (or start-body part-body), true))
            (recur rest-body, cset, next-address, part-address, (conj part-body pexpr), start-body, any-suspend?)))
        ;; return result
        (let* [clean-cset (dissoc-in cset [:bodies start-address])
               cset       (if (> (count part-body) 0)
                            (cset/add clean-cset part-address params part-body)
                            clean-cset)]
          (if any-suspend?
            [start-body, cset, true]
            [body, nil, false]))))))

;;
;; Expressions
;;

(defn partition-expr
  "Breaks an expression down into partitions - blocks of code which are wrapped into
  continuation functions which perform incremental pieces of evaluation.

  Args:
    expr - the expression to be partitioned
    partition = Address of the partition currently being built
    address - Address of the expression
    params - vector of currently bound parameters at this address point
            these become keys when defining continuations
    result-key - where the code should

  Returns:
  [
    start-body - vector of expressions representing the first partition
    cset - a map of addresses to function definitions
    suspending? - boolean indicating whether this expression could suspend the flow;
                  code which uses this expression should wrap the start block
                  inside a (resume-at ...) if there is code which follows this expression.
  ]"
  [expr partition address params]
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (seq? mexpr) (partition-list-expr expr mexpr partition address params)
      (vector? mexpr) (partition-vector-expr expr partition address params)
      (map? mexpr) (partition-map-expr params expr partition address)
      :otherwise [expr, nil, nil])))

(defn partition-list-expr
  [expr mexpr partition address params]
  (let [op (first mexpr)]
    (cond
      (special-symbol? op) (partition-special-expr op expr mexpr partition address params)
      (refers-to? fn? op) (partition-fncall-expr op expr mexpr partition address params)
      (refers-to? flow/flow? op) (partition-flow-expr op expr mexpr partition address params)
      :else (throw (Exception. (format "Unrecognized operator %s in %s" op expr))))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr mexpr partition address params]
  (case op
    let (partition-let-expr expr mexpr partition address params)
    if (partition-if-expr expr mexpr partition address params)
    fn (partition-fn-expr expr mexpr partition address params)
    do (partition-do-expr expr mexpr partition address params)
    quote [expr, nil, false]
    loop (partition-loop-expr expr mexpr address params)
    recur (partition-recur-expr expr mexpr partition address params)
    (throw (Exception. (format "Special operator %s not yet available in Flows in %s" op expr)))))

(defn partition-if-expr
  [expr mexpr partition address params]
  (let [[op test then else] mexpr
        address     (address/child address "if")
        [test-addr, then-addr, else-addr] (map #(address/child address %) ["test" "then" "else"])

        [test-start, test-cset, test-suspend?]
        (partition-expr test, partition, test-addr, params)

        [then-start, then-cset, then-suspend?]
        (partition-expr then, partition, then-addr, params)

        [else-start, else-cset, else-suspend?]
        (partition-expr else, partition, else-addr, params)

        branch-addr (address/child address "branch")

        test-result (gensym "test")
        start       (if test-suspend?
                      `(resume-at [~branch-addr, ~params, ~test-result], test-start)
                      (with-meta `(if, test, then-start, else-start) (meta expr)))
        branch-cset (if test-suspend?
                      (cset/add (cset/create) branch-addr `[~@params ~test-result]
                        `(if, test-result, then-start, else-start)))
        full-cset   (cset/combine test-cset branch-cset then-cset else-cset)
        suspend?    (or test-suspend? then-suspend? else-suspend?)]
    (assert (= op 'if))
    [start, full-cset, suspend?]))

(defn partition-do-expr
  [expr mexpr partition address params]
  (let [[op & body] mexpr
        address (address/child address 'do)
        [bstart, cset, suspend?] (partition-body body partition address params)]
    (assert (= op 'do))
    (if suspend?
      [`(do ~@bstart), cset, suspend?]
      [expr, nil, false])))

(defn partition-loop-expr
  "Partitions a (loop ...) expression, establishing a binding-point for recur. If
  this expression suspends, the start body will be a let construct for establishing
  the initial parameters which calls a partition with a loop, which acts as the binding
  point for continuing the loop. If there are no suspending expressiongs, the expression
  is returned."
  [expr mexpr address params]           ; doesn't take a partition argument
  (let [[op bindings & loop-body] mexpr
        loop-partition    (address/child address 'loop)
        binding-partition (address/child loop-partition 0)

        loop-params       (map first bindings)
        loop-initializers (map second bindings)

        [binding-start, bindings-cset, binding-suspend?]
        (partition-bindings loop-params, loop-initializers, binding-partition, binding-partition, params body)]
    (assert (= op 'loop))
    (with-binding-point [address loop-params] ; partition-recur-expr will use this
      (let [loop-body-params (concat params loop-params)
            [body-start, body-cset, body-suspend?]
            (partition-body loop-body loop-partition loop-partition loop-body-params)
            any-suspend? (or binding-suspend? body-suspend?)]


        (if any-suspend?
          (let [cset (cset/add bindings-cset loop-partition (concat params loop-params) start)]
            [start, cset, true])
          [expr, nil, false])))))

(defn partition-recur-expr
  ;; TODO: need to add partition arg to all partitioner fns
  [expr mexpr partition address params]
  (throw "not implemented")
  (letfn [(make-call [params]
            `(flow/continue ~(:address *recur-binding-point*)
               ~(bindings-expr-from-params params)))]
    (assert *recur-binding-point* (format "No binding point for %s" expr))
    (let [[op & args] mexpr
          loop-address   (:partition *recur-binding-point*)
          same-partition (= partition loop-address)

          [start, cset, suspend?]
          (partition-functional-expr op expr mexpr address params make-call false)]
      (if suspend?
        [start, cset, true]
        (if same-partition
          expr
          [`(flow/continue )])))))

;;
;; Partitioning expressions with bindings
;;

(defn partition-functional-expr
  "Partitions a function-like expression - a list with an operator followed
  by an arbitrary list of arguments.
  make-call-form - is a function which takes a list of parameters and returns
  a form which represents the functional call
  always-suspend - guarantees that the partitioner returns suspend?=true even if none
  of the arguments suspend."
  [op expr mexpr address params make-call-form always-suspend?]
  (let [address    (address/child address op)
        [_ & args] mexpr
        keys       (nsymbols (count args))
        pcall-body (with-meta (make-call-form keys) (meta expr))
        [start, cset, suspend?]
        (partition-bindings keys args address params
          `[~pcall-body])]
    (if suspend?
      [start, cset, true]
      [(make-call-form args), nil, always-suspend?])))

(defn partition-vector-expr
  [expr address params]
  (partition-functional-expr "[]" expr expr address params
    #(`(vector ~@%)) true))

(defn partition-flow-expr
  [op expr mexpr address params]
  (partition-functional-expr op expr mexpr address params
    #(cons `flow/start (cons op %)), true))

(defn partition-fncall-expr
  [op, expr, mexpr, partition, address, params]
  (partition-functional-expr op expr mexpr address params
    #(cons op %), false))

(defn partition-bindings
  "Partitions the bindings, and executes the body in that context. Note:
  this function does not partition the body. The caller should ensure that
  with partition body and supply the `start` result as body.
  Args:
  Returns: [start, cset, suspend?]"
  [keys, args, partition, address, params, body]
  (loop
    [[key & rest-keys] keys
     [arg & rest-args] args
     cur-part-address  (address/child address 0)
     cur-part-bindings []
     part-params       params           ; params provided to this partition
     start             nil
     any-suspend?      false
     cset              (cset/create)]
    (if key
      (let [binding-address cur-part-address
            next-address    (address/increment binding-address)
            [arg-start, arg-cset, suspend?] (partition-expr arg, partition, binding-address, params)
            cset            (cset/combine cset arg-cset)]
        (if suspend?
          (let [cur-part-params (map first cur-part-bindings)
                new-params      `[~@params ~@cur-part-params]
                resume-pbody    `(resume-at [~next-address ~new-params ~key]
                                   ~arg-start)
                pbody           (if (> (count cur-part-bindings) 0)
                                  `(let ~cur-part-bindings ~resume-pbody)
                                  resume-pbody)
                cset            (if start (cset/add cset cur-part-address part-params pbody) cset)
                start           (or start pbody)]
            (recur rest-keys, rest-args, next-address, [],
              new-params, start, (or suspend? any-suspend?) cset))
          (recur rest-keys, rest-args
            cur-part-address,
            `[~@cur-part-bindings [~key ~arg]],
            params, start, any-suspend?, cset)))

      ;; finalize the last partition by executing the body with the
      ;; bound params
      (let [final-body (if (> (count cur-part-bindings) 0) `(let, cur-part-bindings, @body) body)
            cset       (cset/add cset cur-part-address params final-body)
            result     [start, cset, any-suspend?]]
        ;; return result
        result))))

;; HELPERS
(defn macroexpand-keeping-metadata
  [expr]
  (let [expr-meta (meta expr)
        mexpr     (macroexpand expr)]
    (if expr-meta
      (with-meta mexpr expr-meta)
      mexpr)))

(defn nsymbols
  ([n] (nsymbols n 0))

  ([n start]
   (map #(gensym (str "arg" %)) (range start n))))

(defn bindings-expr-from-params
  [params]
  `(hash-map ~@(flatten #([(key %) %]) params)))
;
;(defn constant?
;  "True if x represents a constant value"
;  [x]
;  (or (string? x) (keyword? x) (number? x) (char? x)
;    (contains? [true false nil ##Inf ##-Inf ##NaN] x)
;    (and (list? x) (= (first x) 'quote))))

