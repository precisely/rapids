(ns longterm.partition
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]]
            [longterm.flow :as flow]
            [longterm.continuation_set :as cset])
  (:use longterm.stack))

(declare partition-body partition-expr partition-fncall-expr
  partition-list-expr partition-flow-expr partition-body-with-bindings
  partition-if-expr partition-let-expr partition-fn-expr
  partition-do-expr partition-loop-expr partition-special-expr partition-recur-expr
  partition-vector-expr partition-map-expr macroexpand-keeping-metadata
  nsymbols)

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
  [body                                 ; list of expressions which should be partitiond
   address                              ; address of the current body; body forms will be address/0, address/1 etc
   params]                              ; vector of symbols which will be bound on entry
  "Returns [start-body, cset, suspend?]"
  (loop [[expr & rest-body] body
         cset         (cset/create)
         part-address (address/child address 0)
         part-body    []
         start-body   []
         any-suspend? nil]
    (if body
      (let [[pexpr expr-cset suspend?] (partition-expr expr part-address params)
            cset (cset/combine cset expr-cset)]
        (if suspend?
          (let [next-address (address/increment address)
                part-body    (conj part-body `(resume-at [~next-address ~params] ~@pexpr))
                cset         (cset/add cset part-address params part-body)]
            (recur rest-body cset next-address [] (or start-body part-body) true))
          (recur rest-body cset part-address part-body start-body any-suspend?)))
      ;; return result
      [start-body, cset, any-suspend?])))

;;
;; Expressions
;;

(defn partition-expr
  "Breaks an expression down into partitions - blocks of code which are wrapped into
  continuation functions which perform incremental pieces of evaluation.

  Args:
    expr - the expression to be partitioned
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
  [expr address params]
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (list? mexpr) (partition-list-expr expr mexpr address params)
      (vector? mexpr) (partition-vector-expr expr address params)
      (map? mexpr) (partition-map-expr params expr address)
      :otherwise expr)))

(defn partition-list-expr
  [expr mexpr address params]
  (let [op (first mexpr)]
    (cond
      (special-symbol? op) (partition-special-expr op expr mexpr address params)
      (refers-to? fn? op) (partition-fncall-expr op expr mexpr address params)
      (refers-to? flow/flow? op) (partition-flow-expr op expr mexpr address params)
      :else (throw (Exception. (format "Unrecognized operator %s in %s" op expr))))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr mexpr address params]
  (case op
    let (partition-let-expr expr mexpr address params)
    if (partition-if-expr expr mexpr address params)
    fn (partition-fn-expr expr mexpr address params)
    do (partition-do-expr expr mexpr address params)
    quote [expr, nil, false]
    loop (partition-loop-expr expr mexpr address params)
    (throw (Exception. (format "Special operator %s not yet available in Flows in %s" op expr)))))

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
  (let [address (address/child address op)
        [_ & args] mexpr
        keys       (nsymbols (count args))
        pcall-body (with-meta (make-call-form keys) expr)
        [start, cset, suspend?]
        (partition-body-with-bindings keys args address params
          `[~pcall-body])]
    (if suspend?
      [start, cset, true]
      [pcall-body , nil, always-suspend?])))

(defn partition-vector-expr
  [expr address params]
  (partition-functional-expr "[]" expr expr address params
    #(`(vector ~@%)) true))

(defn partition-flow-expr
  [op expr mexpr address params]
  (partition-functional-expr op expr mexpr address params
    #(`(flow/start '~op ~%)), true))

(defn partition-fncall-expr
  [op, expr, mexpr, address, params]
  (partition-functional-expr op expr mexpr address params
    #(`(~op ~%)), false))

(defn partition-body-with-bindings
  [keys, args, address, params, body]
  (loop
    [[key & rest-keys] keys
     [arg & rest-args] args
     cur-part-address  (address/child address 0)
     cur-part-bindings []
     part-params       params           ; params provided to this partition
     start             nil
     any-suspend?      nil
     cset              (cset/create)]
    (if args
      (let [binding-address cur-part-address
            next-address    (address/increment binding-address)
            [arg-start, arg-cset, suspend?] (partition-expr arg binding-address params)
            cset            (cset/combine cset arg-cset)]
        (if suspend?
          (let [cur-part-params (map first cur-part-bindings)
                new-params      `[~@params ~@cur-part-params]
                resume-pbody    `(resume-at [~next-address ~new-params ~key]
                                   ~arg-start)
                pbody           (if (> cur-part-bindings 0)
                                  `(let ~cur-part-bindings ~resume-pbody)
                                  resume-pbody)
                cset            (if start (cset/add cset cur-part-address part-params pbody) cset)
                start           (or start pbody)]
            (recur rest-keys, rest-args, next-address, [],
              new-params, start, (or suspend? any-suspend?) cset))
          (recur rest-keys, rest-args
            cur-part-address,
            `[~@cur-part-bindings [key arg]],
            params, start, any-suspend?, cset)))

      ;; finalize the last partition by executing the body with the
      ;; bound params
      (let [final-body `(let, cur-part-bindings, @body)
            cset       (cset/add cset cur-part-address params final-body)]
        ;; return result
        [start, cset, any-suspend?]))))

;; HELPERS
(defn macroexpand-keeping-metadata
  [expr]
  (let [expr-meta (meta expr)
        mexpr (macroexpand expr)]
    (if expr-meta
      (vary-meta  merge (meta expr))
      mexpr)))

(defn nsymbols
  ([n] (nsymbols n 0))

  ([n start]
   (map #(gensym (str "arg" %)) (range start n))))

;
;(defn constant?
;  "True if x represents a constant value"
;  [x]
;  (or (string? x) (keyword? x) (number? x) (char? x)
;    (contains? [true false nil ##Inf ##-Inf ##NaN] x)
;    (and (list? x) (= (first x) 'quote))))


;(defn partition-let-expr
;  [expr mexpr address params]
;  (let [address  (address/child address 'let)
;        bindings (second mexpr)
;        keys     (map first bindings)
;        vexprs   (map second bindings)
;        body     (nthrest mexpr 2)
;
;        [start, cset, suspend?]
;        (partition-body-with-bindings keys vexprs address params body)]
;
;    (if suspend?
;      [start, cset, suspend?]
;      [expr, nil, nil])))
;
;(defn partition-fncall-expr
;  [op, expr, mexpr, address, params]
;  (let [address (address/child address op)
;        [_ & args] mexpr
;        keys (nsymbols (count args))
;        pcall-body (with-meta `(~op ~@keys) (meta expr))
;        [start, cset, suspend?]
;        (partition-body-with-bindings keys args address params
;          `[~pcall-body])]
;    (if suspend?
;      [start, cset, true]
;      [pcall-body, nil, nil])))
