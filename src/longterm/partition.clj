(ns longterm.partition
  (:require [longterm.address :as address]
            [longterm.util :refer :all]
            [longterm.flow :as flow]
            [longterm.recur :refer [with-tail-position with-binding-point]]
            [longterm.partition-set :as pset]
            [longterm.runloop :refer [resume-at bindings-expr-from-params]]
            [longterm.recur :refer :all])
  (:import (longterm.address Address)))

;;;; Partitioner

;;;
;;; Breaks flow bodies into partitions representing sequences of expressions
;;; which can be executed without being suspended.
;;;
;;; Partitions are used to define continuations. Breaks are introduced by calls
;;; to flows or to the `wait-for` function, which may only appear inside
;;; flow bodies.
;;;
;;; When a flow is called, a StackFrame is created which captures the bindings
;;; and the address where execution should resume. When a `(wait-for event-id)`
;;; expression is encountered, the stack of thunks is persisted
;;; to a non-volatile storage and associated with these values. When an event
;;; matching the current `*run-id*` and `event-id` is received, the thunk stack is
;;; retrieved, and execution is resumed at the point in the flow after the
;;; `(wait-for)` call, with the bindings that were present when `(wait-for...)`
;;; was invoked. At the end of each continuation, the thunk at the top of
;;; the stack is popped and execution continues at the address in the next
;;; thunk.
;;;

;;; A small, incomplete example:
;;;
;;; ```
;;; (deflow main [arg]
;;;   (let [result (flow2 arg)]
;;;     (print "(flow2 %s) = %s" arg result)))
;;; ```
;;;
;;; gets broken into 2 partitions, named by addresses (shown in angle brackets).
;;; The partitions contain all the information necessary for generating
;;; continuations, the functions that actually implement a flow.
;;;
;;; <a1> => ```(fn [& {:keys [arg]}] (resume-at [<a2> {:arg arg} 'result] (flow2 arg)))```
;;; <a2> => ```(fn [& {:keys [arg result]}] (print "(flow2 %s) = %s" arg result))```
;;;
;;; The addresses represented by <p1> and <p2> are records that describe a unique point
;;; in the flow.
;;;
;;; E.g., <a1> is #longterm.address.Address[main []], representing the beginning of
;;; the main flow.
;;;

(declare partition-body partition-expr partition-fncall-expr
         partition-list-expr partition-flow-expr
         partition-functional-expr partition-bindings
         partition-if-expr partition-let-expr partition-fn*-expr
         partition-do-expr partition-loop-expr partition-special-expr partition-recur-expr
         partition-vector-expr partition-map-expr macroexpand-keeping-metadata
         partition-suspend-expr throw-partition-error
         nsymbols)

(defn partition-body
  "Partitions a list of expressions, e.g., for do, let and deflow forms
  Args:
   body     - list of expressions which should be partitiond
   partition - the current external partition address
   address  - address of the current body; body forms will be address/0, address/1 etc
   params   - vector of symbols which will be bound on entry

  Returns [
    start-body - forms which begin the execution of the body
    pset,      - partition set
    suspend?]  - true if any nested expression suspends execution
  "
  ;; Note: forms are not added to the initial partition-address. They are provided
  ;; in the `start` return value, and will be incorporated into partition-address
  ;; by the caller. This is necessary because the caller knows where to
  ;; store the initial forms. For example, consider branching code like:
  ;; ```
  ;; (if (some-test)
  ;;    (do (fn1) (flow1) (flow2)) ; then
  ;;    (do (fn2) (flow3) (flow4))) ; else
  ;; ```
  ;; The starting partition needs to contain:
  ;; ```
  ;; (if (some-test)
  ;;   (do (fn1) (resume-at [...] (flow1))
  ;;   (do (fn2) (resume-at [...] (flow3)))
  ;; ```
  ;; The if-partitioner ends up calling partition-body twice, pasting the
  ;; results
  ([body partition-address address params]
   {:post [(vector (first %))]}
   (let [dirty-address partition-address]
     (loop [iter-body body
            pset (pset/create)
            cur-address (address/child address 0)
            partition-address partition-address
            part-body []
            start-body nil
            any-suspend? false]
       (if-not (empty? iter-body)
         (let [[expr & rest-body] iter-body
               [pexpr expr-pset suspend?] (with-tail-position [(empty? rest-body)]
                                            (partition-expr expr partition-address cur-address params))
               pset (pset/combine pset expr-pset)
               next-address (address/increment cur-address)]
           (if suspend?
             (let [final-part-expr (if (> (count rest-body) 0) `(resume-at [~next-address [~@params] nil] ~pexpr) pexpr)
                   part-body (conj part-body final-part-expr)
                   pset (pset/add pset partition-address params part-body)]

               (recur rest-body, pset, next-address, next-address, [], (or start-body part-body), suspend?))
             (recur rest-body, pset, next-address, partition-address, (conj part-body pexpr), start-body, any-suspend?)))

         ;; return result
         (let* [clean-pset (pset/delete pset dirty-address)
                pset (if (> (count part-body) 0)
                       (pset/add clean-pset partition-address params part-body)
                       clean-pset)]
           (if any-suspend?
             [start-body, pset, any-suspend?]
             [body, nil, false])))))))

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
    start       - an s-expression which starts computation of expr
    pset        - a map of addresses to function definitions
    suspend?    - boolean indicating whether this expression could suspend the flow
  ]"
  [expr partition-addr address params]
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (seq? mexpr) (partition-list-expr expr mexpr partition-addr address params)
      (vector? mexpr) (partition-vector-expr expr partition-addr address params)
      (map? mexpr) (partition-map-expr expr partition-addr address params)
      :otherwise [expr, nil, nil])))

(defn special-form-op? [x] (in? '[if do let let* fn fn* loop loop* quote recur] x))
(defn partition-list-expr
  [expr mexpr partition-addr address params]
  (let [op (first expr)
        mop (first mexpr)
        ops [op mop]]
    (cond
      ;; attempt to detect operator in expression
      (some special-form-op? ops) (partition-special-expr mop expr mexpr partition-addr address params)
      ;; note: the following are not special-symbols, though the docs list them as special forms:
      (some suspend-op? ops) (partition-suspend-expr expr mexpr partition-addr address params)
      (some #(refers-to? flow/flow? %) ops) (partition-flow-expr mop expr mexpr partition-addr address params)
      :else (partition-fncall-expr mop expr mexpr partition-addr address params))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr mexpr partition-addr address params]
  (case op
    if (partition-if-expr expr mexpr partition-addr address params)
    do (partition-do-expr expr mexpr partition-addr address params)
    let (partition-let-expr expr mexpr partition-addr address params)
    let* (partition-let-expr expr mexpr partition-addr address params)
    fn (partition-fn*-expr expr mexpr partition-addr address params)
    fn* (partition-fn*-expr expr mexpr partition-addr address params)
    loop (partition-loop-expr expr mexpr address params)
    loop* (partition-loop-expr expr mexpr address params)
    quote [expr, nil, false]
    recur (partition-recur-expr expr mexpr partition-addr address params)
    (throw-partition-error "Special operator not yet available in flows" expr "Operator: %s" op)))

(defn partition-fn*-expr
  "Throws if fn* contain suspending ops - this partitioner acts as a guard
  against knowing or inadvertent inclusion of a suspending operation inside a fn.
  This might happen inadvertently if the user uses a macro (like for) which
  expands into a (fn ...) expression. This is a bit of a blunt instrument,
  but not sure what else to do right now."
  [expr mexpr partition-addr address params]
  (letfn [(sig-suspends? [sig]
            (let [body (rest sig)
                  [_, _, suspend?] (partition-body body partition-addr address params)]
              (if suspend?
                (throw-partition-error "Illegal attempt to suspend in function body" expr))
              suspend?))]
    (let [sigs (rest mexpr)
          name (if (symbol? (first sigs)) (first sigs) nil)
          sigs (if name (next sigs) sigs)
          sigs (if (vector? (first sigs))
                 (list sigs)
                 sigs)
          suspending? (some #(sig-suspends? %) sigs)]
      (assert (not suspending?))
      [expr, nil, false])))

(defn partition-let-expr
  [expr mexpr partition-addr address params]
  (let [expr-op (first expr)
        address (address/child address 'let)
        binding-address (address/child address 0)
        body-address (address/child address 1)
        [_ bindings & body] (if (= expr-op 'let) expr mexpr)
        [keys, args] (map vec (reverse-interleave bindings 2))

        [body-start, body-pset, body-suspend?]
        (partition-body body, partition-addr, body-address, (concat params keys))

        [bind-start, bind-pset, bind-suspend?]
        (partition-bindings keys, args, partition-addr, binding-address, params,
                            (if body-suspend? body-start (vec body)))

        pset (pset/combine body-pset bind-pset)]
    (cond
      bind-suspend?
      [bind-start, pset, true]

      body-suspend?
      [(with-meta `(let ~bindings ~@body-start) (meta expr)), pset, true]

      :else [expr, nil, false])))

(defn partition-if-expr
  [expr mexpr partition-addr address params]
  (let [[_ test then else] mexpr
        address (address/child address 'if)
        [test-addr, then-addr, else-addr] (map #(address/child address %) [0 1 2])

        [test-start, test-pset, test-suspend?]
        (with-tail-position [false]
          (partition-expr test, partition-addr, test-addr, params))

        [then-start, then-pset, then-suspend?]
        (partition-expr then, partition-addr, then-addr, params)

        [else-start, else-pset, else-suspend?]
        (partition-expr else, partition-addr, else-addr, params)

        branch-addr address

        test-result (first (nsymbols address 1))
        start (if test-suspend?
                `(resume-at [~branch-addr, [~@params], ~test-result], ~test-start)
                (with-meta `(if ~test ~then-start ~else-start) (meta expr)))
        branch-pset (if test-suspend?
                      (pset/add (pset/create) branch-addr `[~@params ~test-result]
                                `[(if ~test-result ~then-start ~else-start)]))
        full-pset (pset/combine test-pset branch-pset then-pset else-pset)
        suspend? (or test-suspend? then-suspend? else-suspend?)]
    [start, full-pset, suspend?]))

(defn partition-do-expr
  [expr mexpr partition address params]
  (let [[op & body] mexpr
        address (address/child address 'do)
        [bstart, pset, suspend?] (partition-body body partition address params)]
    (assert (= op 'do))
    (if suspend?
      [`(do ~@bstart), pset, suspend?]
      [expr, nil, false])))

(defn partition-loop-expr
  "Partitions a (loop ...) expression, establishing a binding-point for recur. If
  this expression suspends, the start body will be a let construct for establishing
  the initial parameters which calls a partition with a loop, which acts as the binding
  point for continuing the loop. If there are no suspending expressiongs, the expression
  is returned.

  Returns [start, pset, suspend?]"
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
  (let [op (first expr)
        [_ bindings & loop-body] (if (in? '[loop loop*] op) expr mexpr)
        form-address (address/child address 'loop)
        binding-partition (address/child form-address 0)
        loop-partition (address/child form-address 1)

        [loop-params, loop-initializers] (reverse-interleave bindings 2)]

    ;; we need to partition the loop body FIRST, then pass the start-body to partition-bindings
    (let [loop-body-params (concat params loop-params)

          ;; partition the loop body with the loop-partition registered as a recur
          ;; binding point; if recur calls happen within this partition, they
          ;; are treated as regular recur calls; otherwise, the recur is being executed
          ;; in a resumed runlet, so a call to the loop-partition is generated.
          [start-body, body-pset, body-suspend?]
          (with-tail-position [:reset]
            (with-binding-point [loop-partition loop-params] ; partition-recur-expr will use this
              (partition-body loop-body loop-partition loop-partition loop-body-params)))

          ;; the loop merely rebinds the loop parameters provided by the continuation
          ;; note: we need a normal (loop ...) expression because we may have normal non-suspending
          ;; recur expressions in addition to suspending recur expressions.
          loop-partition-body [`(loop [~@(apply concat (map #(vector % %) loop-params))] ~@start-body)]
          loop-partition-params (concat loop-params params)
          loop-pset (pset/add (pset/create) loop-partition loop-partition-params loop-partition-body)

          flow-continue-bindings (bindings-expr-from-params loop-partition-params)
          binding-body `[(flow/continue ~loop-partition ~flow-continue-bindings)]

          ;; now we can compute the initializers and give it the partitioned loop start...
          [binding-start, bindings-pset, binding-suspend?]
          (partition-bindings loop-params, loop-initializers, binding-partition, binding-partition, params, binding-body)

          any-suspend? (or binding-suspend? body-suspend?)

          pset (pset/combine body-pset bindings-pset loop-pset)]
      (if any-suspend?
        [binding-start, pset, any-suspend?]
        [expr, nil, false]))))

(defn partition-recur-expr
  "Returns:
  [start, pset, suspend?]"
  [expr, mexpr, partition-address, address, params]
  (letfn [(make-call [params]
            (let [loop-params (:params *recur-binding-point*)
                  bindings `(hash-map ~@(interleave (map #(list 'quote %) loop-params) params))]
              (if-not (= (count params) (count loop-params))
                (throw-partition-error "Mismatched argument count to recur" expr "expected: %s args, got: %s"
                                       (count params) (count loop-params)))
              `(flow/continue ~(:address *recur-binding-point*)
                              ~bindings)))]
    (if-not *recur-binding-point*
      (throw-partition-error "No binding point" expr))
    (if-not *tail-position*
      (throw-partition-error "Can only recur from tail position" expr))

    (let [[op & args] mexpr
          loop-address (:address *recur-binding-point*)

          address (address/child address 'recur)

          [start, pset, suspend?]
          (partition-functional-expr op, expr, mexpr, partition-address, address, params, make-call)

          same-partition (and (false? suspend?) (= partition loop-address))]
      (if same-partition
        [expr, nil, false, true]                            ; plain vanilla recur!
        (if suspend?
          [start, pset, true]
          (if same-partition
            [expr, nil, false]
            [(make-call args), pset, false]))))))

;;
;;
;;
(defn partition-suspend-expr
  [expr, mexpr, partition-addr, address, params]
  (let [[start, pset, _]
        (partition-functional-expr 'suspend! expr mexpr partition-addr address params
                                   #(cons 'suspend! %))]
    [start, pset, true]))

;;
;; Partitioning expressions with bindings
;;

(defn partition-functional-expr
  "Partitions a function-like expression - a list with an operator followed
  by an arbitrary list of arguments.
  make-call-form - is a function which takes a list of parameters and returns
  a form which represents the functional call"
  [op, expr, mexpr, partition-address, address, params, make-call-form]
  (letfn [(call-form [args]
            (with-meta (make-call-form args) (meta expr)))]
    (let [address (address/child address op)
          [_ & args] mexpr
          keys (nsymbols address (count args))
          pcall-body [(call-form keys)]
          [start, pset, suspend?]
          (partition-bindings keys args partition-address address params
                              pcall-body)]
      (if suspend?
        [start, pset, suspend?]                             ; partition bindings has provided the correct result
        [(call-form args), nil, false]))))

(defn partition-map-expr
  [expr partition-addr address params]
  {:pre [(map? expr)
         (instance? Address partition-addr)
         (instance? Address address)
         (vector? params)]}
  ;; TODO: this could be simplified...
  (let [fake-op (symbol "_map_")
        pseudo-expr (cons fake-op (apply concat (map identity expr)))
        [start, pset, suspend?] (partition-functional-expr fake-op pseudo-expr pseudo-expr partition-addr address params
                                                           (fn [args] (into {} (map vec (partition 2 args)))))]
    (if suspend?
      [start, pset suspend?]
      [expr, nil, false])))

(defn partition-vector-expr
  [expr partition-addr address params]
  (let [fake-op (symbol "[]")
        expr-with-op (with-meta `(~fake-op ~@expr) (meta expr))]

    ;[start, pset, suspend?]
    (partition-functional-expr fake-op expr-with-op expr-with-op partition-addr address params
                               #(vec %))))

(defn partition-flow-expr
  [op, expr, mexpr, partition-addr, address, params]
  (let [[start, pset, _] (partition-functional-expr op expr mexpr partition-addr address params
                                                    (fn [args]
                                                      `(flow/start ~op ~@args)))]
    [start, pset, true]))

(defn partition-fncall-expr
  [op, expr, mexpr, partition-addr, address, params]
  (partition-functional-expr op expr mexpr partition-addr address params
                             (fn [args] `(~op ~@args))))

(defn partition-bindings
  "Partitions the bindings, and executes `body` in that context. Note:
  this function does not partition `body` - it merely pastes the supplied
  forms into the appropriate location after the bindings.

  The caller should separately partition forms and supply the `start` result
  as the `body` argument.

  Args:
  Returns: [start, pset, suspend?]"
  [keys, args, partition-address, address, params, body]
  {:pre [(address/address? partition-address)
         (address/address? address)
         (vector? body)]}
  (with-tail-position [false]
    (let [dirty-address partition-address]
      (loop
        [[key & rest-keys] keys
         [arg & rest-args] args
         current-bindings []                                ; the new bindings introduced to the partition
         partition-address partition-address
         arg-address (address/child address 0)              ; address of the arg
         part-params params                                 ; params provided to this partition
         start nil
         any-suspend? false
         pset (pset/create)]
        (if key
          (let [[arg-start, arg-pset, suspend?]
                (partition-expr arg, partition-address, arg-address, params) ;

                pset (pset/combine pset arg-pset)
                next-address (address/increment arg-address)]
            (if suspend?
              (let [cur-part-params (map first current-bindings)
                    resume-params (concat part-params, cur-part-params)
                    resume-pexpr `(resume-at [~next-address [~@resume-params] ~key]
                                             ~arg-start)
                    new-params (if key (conj resume-params key) resume-params) ; the next partition's params includes the key
                    let-bindings (apply concat current-bindings)
                    pexpr (if (> (count current-bindings) 0)
                            `(let [~@let-bindings] ~resume-pexpr)
                            resume-pexpr)
                    pset (pset/add pset partition-address part-params [pexpr])
                    start (or start pexpr)]
                (recur rest-keys, rest-args, [], next-address, next-address,
                       new-params, start, (or suspend? any-suspend?), pset))
              (do
                (recur rest-keys, rest-args
                       (conj current-bindings [key arg]),
                       partition-address, next-address,
                       part-params, start, any-suspend?, pset))))

          ;; finalize the last partition by executing the body with the
          ;; bound params
          (let [let-bindings (apply concat current-bindings)
                final-body (if (> (count current-bindings) 0) [`(let [~@let-bindings] ~@body)] (vec body))
                clean-pset (pset/delete pset dirty-address)
                start (or start (first final-body))
                pset (pset/add clean-pset partition-address part-params final-body)]
            [start, pset, any-suspend?]))))))

;; HELPERS
(defn macroexpand-keeping-metadata
  [expr]
  (let [expr-meta (meta expr)
        mexpr (macroexpand expr)]
    (if expr-meta
      (with-meta mexpr expr-meta)
      mexpr)))

(defn nsymbols
  ([address n] (nsymbols address n 0))

  ([address n start]
   (let [base (str "p|" (clojure.string/join "-" (:point address)))]
     (map #(symbol (str base "|" %)) (range start n)))))

(defn throw-partition-error
  ([name expr] (throw-partition-error name expr nil))
  ([name expr msg & args]
   (let [loc (ifit (:line (meta expr)) (format " (line %s)" it) "")
         extra-info (if msg (str ". " (apply format msg args)) "")]
     (throw (Exception. (format "%s%s%s: %s" name loc extra-info expr))))))
