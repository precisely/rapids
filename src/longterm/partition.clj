(ns longterm.partition
  (:require [longterm.address :as address]
            [longterm.util :as util :refer [refers-to?]]
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
  [body partition-address address params]
  "Partitions a list of expressions, e.g., for do, let and deflow forms
  Args:
   body     - list of expressions which should be partitiond
   partition - the current external partition address
   address  - address of the current body; body forms will be address/0, address/1 etc
   params   - vector of symbols which will be bound on entry

  Returns [start-body, cset, suspend?]"
  (let [dirty-address partition-address] ; remove from cset before returning
    (loop [iter-body         body
           cset              nil
           cur-address       (address/child address 0)
           partition-address partition-address
           part-body         []
           start-body        nil
           any-suspend?      false]
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
            (recur rest-body, cset, next-address, partition-address, (conj part-body pexpr), start-body, any-suspend?)))
        ;; return result
        (let* [clean-cset (cset/delete cset dirty-address)
               cset       (if (> (count part-body) 0)
                            (cset/add clean-cset partition-address params part-body)
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
  [expr partition-addr address params]
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (seq? mexpr) (partition-list-expr expr mexpr partition-addr address params)
      (vector? mexpr) (partition-vector-expr expr partition-addr address params)
      (map? mexpr) (partition-map-expr params expr partition-addr address)
      :otherwise [expr, nil, nil])))

(defn partition-list-expr
  [expr mexpr partition-addr address params]
  (let [op (first mexpr)]
    (cond
      (special-symbol? op) (partition-special-expr op expr mexpr partition-addr address params)
      (util/refers-to? fn? op) (partition-fncall-expr op expr mexpr partition-addr address params)
      (util/refers-to? flow/flow? op) (partition-flow-expr op expr mexpr partition-addr address params)
      :else (throw (Exception. (format "Unrecognized operator %s in %s" op expr))))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr mexpr partition-addr address params]
  (case op
    let (partition-let-expr expr mexpr partition-addr address params)
    if (partition-if-expr expr mexpr partition-addr address params)
    fn (partition-fn-expr expr mexpr partition-addr address params)
    do (partition-do-expr expr mexpr partition-addr address params)
    quote [expr, nil, false]
    loop (partition-loop-expr expr mexpr address params)
    recur (partition-recur-expr expr mexpr partition-addr address params)
    (throw (Exception. (format "Special operator %s not yet available in Flows in %s" op expr)))))

(defn partition-if-expr
  [expr mexpr partition-addr address params]
  (let [[op test then else] mexpr
        address     (address/child address "if")
        [test-addr, then-addr, else-addr] (map #(address/child address %) ["test" "then" "else"])

        [test-start, test-cset, test-suspend?]
        (partition-expr test, partition-addr, test-addr, params)

        [then-start, then-cset, then-suspend?]
        (partition-expr then, partition-addr, then-addr, params)

        [else-start, else-cset, else-suspend?]
        (partition-expr else, partition-addr, else-addr, params)

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
  is returned.

  Returns [start, cset, suspend?]"
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
  ;;                    around (flow/continue <loop-partition/1> ...) where
  ;;                    the bindings contain the updated loop variables
  [expr mexpr address params]
  (let [[op bindings & loop-body] mexpr
        form-address      (address/child address 'loop)
        binding-partition (address/child form-address 0)
        loop-partition    (address/child form-address 1)

        loop-params       (map first bindings)
        loop-initializers (map second bindings)]
    (assert (= op 'loop))

    ;; we need to partition the loop body FIRST, then pass the start-body to partition-bindings
    (let [loop-body-params (concat params loop-params)

          ;; partition the loop body with the loop-partition registered as a recur
          ;; binding point; if recur calls happen within this partition, they
          ;; are treated as regular recur calls; otherwise, the recur is being executed
          ;; in a resumed runlet, so a call to the loop-partition is generated.
          [start-body, body-cset, body-suspend?]
          (with-binding-point [loop-partition loop-params] ; partition-recur-expr will use this
            (partition-body loop-body loop-partition loop-partition loop-body-params))

          loop-body        `(loop [~@(flatten (map #([% %]) loop-params))]
                              ~start-body)

          ;; now we can compute the initializers give it the partitioned loop start...
          [binding-start, bindings-cset, binding-suspend?]
          (partition-bindings loop-params, loop-initializers, binding-partition, binding-partition, params, loop-body)

          any-suspend?     (or body-suspend? binding-suspend?)

          cset             (cset/combine body-cset bindings-cset)]
      (if any-suspend?
        [binding-start, cset, true]
        [expr, nil, false]))))

(defn partition-recur-expr
  "Returns:
  [start, cset, suspend?]"
  [expr, mexpr, partition-address, address, params]
  (letfn [(make-call [params]
            (let [loop-params (:params *recur-binding-point*)]
              (assert (= (count params) (count loop-params))
                (format "Mismatched argument count to recur, expected: %s args, got: %s"
                  (count params) (count loop-params))))
            `(flow/continue ~(:address *recur-binding-point*)
               (hash-map ~@(flatten (map #([(key %) %]) params)))))]
    (assert *recur-binding-point* (format "No binding point for %s" expr))
    (assert *tail-position* (format "Can only recur from tail position: " expr))
    (let [[op & args] mexpr
          loop-address   (:partition *recur-binding-point*)

          address        (address/child address 'recur)
          recur-body     `(flow/start ~loop-address ~@args)

          [start, cset, suspend?]
          (partition-functional-expr op, expr, mexpr, partition-address, address, params, make-call, false)

          same-partition (and (false? suspend?) (= partition loop-address))]
      (if same-partition
        [expr, nil, false, true]        ; plain vanilla recur!
        (if suspend?
          [start, cset, true]
          (if same-partition
            expr
            [recur-body]))))))

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
  [op, expr, mexpr, partition-address, address, params, make-call-form, always-suspend?]
  (let [address    (address/child address op)
        [_ & args] mexpr
        keys       (nsymbols (count args))
        pcall-body (with-meta (make-call-form keys) (meta expr))
        [start, cset, suspend?]
        (partition-bindings keys args partition-address address params
          `[~pcall-body])]
    (if suspend?
      [start, cset, true]
      [(make-call-form args), nil, always-suspend?])))

(defn partition-vector-expr
  [expr partition-addr address params]
  (partition-functional-expr "[]" expr expr partition-addr address params
    #(`[~@%]), true))

(defn partition-flow-expr
  [op, expr, mexpr, partition-addr, address, params]
  (partition-functional-expr op expr mexpr partition-addr address params
    #(cons `flow/start (cons op %)), true))

(defn partition-fncall-expr
  [op, expr, mexpr, partition-addr, address, params]
  (partition-functional-expr op expr mexpr partition-addr address params
    #(cons op %), false))

(defn partition-bindings
  "Partitions the bindings, and executes `body` in that context. Note:
  this function does not partition `body` - it merely pastes the supplied
  forms into the appropriate location after the bindings.

  The caller should separately partition forms and supply the `start` result
  as the `body` argument.

  Args:
  Returns: [start, cset, suspend?]"
  [keys, args, partition-address, address, params, body]
  (let [dirty-address partition-address]
  (with-tail-position [false]
    (loop
      [[key & rest-keys] keys
       [arg & rest-args] args
       current-bindings  []             ; the new bindings introduced to the partition
       partition-address partition-address
       arg-address       (address/child address 0) ; address of the arg
       part-params       params         ; params provided to this partition
       start             nil
       any-suspend?      false
       cset              (cset/create)]
      (if key
        (let [next-address (address/increment arg-address)

              [arg-start, arg-cset, suspend?]
              (partition-expr arg, partition-address, arg-address, params) ;

              cset         (cset/combine cset arg-cset)]
          (if suspend?
            (let [cur-part-params (map first current-bindings)
                  new-params      `[~@params ~@cur-part-params]
                  resume-pbody    `(resume-at [~next-address ~new-params ~key]
                                     ~arg-start)
                  pbody           (if (> (count current-bindings) 0)
                                    `(let [~@current-bindings] ~resume-pbody)
                                    resume-pbody)
                  cset            (cset/add cset partition-address part-params pbody)
                  start           (or start pbody)]
              (recur rest-keys, rest-args, [], next-address, next-address,
                new-params, start, (or suspend? any-suspend?), cset))
            (recur rest-keys, rest-args
              (conj current-bindings [key arg]),
              partition-address, next-address,
              params, start, any-suspend?, cset)))

        ;; finalize the last partition by executing the body with the
        ;; bound params
        (let [final-body (if (> (count current-bindings) 0) `(let [~@current-bindings] ~@body) body)
              clean-cset (cset/delete cset dirty-address)
              cset       (cset/add clean-cset partition-address params final-body)]
          [start, cset, any-suspend?]))))))

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
  `(hash-map ~@(flatten (map #([(key %) %]) params))))

;
;(defn constant?
;  "True if x represents a constant value"
;  [x]
;  (or (string? x) (keyword? x) (number? x) (char? x)
;    (contains? [true false nil ##Inf ##-Inf ##NaN] x)
;    (and (list? x) (= (first x) 'quote))))
