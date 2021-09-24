(ns rapids.partitioner.partition
  (:require [rapids.objects.address :as a]
            [rapids.objects.flow :as flow]
            [rapids.objects.closure :refer [->Closure]]
            [rapids.objects.signals :refer [suspending-operator?]]
            [rapids.partitioner.closure :as closure]
            [rapids.partitioner.partition-set :as pset]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.partitioner.recur :refer [with-tail-position with-binding-point *recur-binding-point* *tail-position*]]
            [rapids.support.util :refer :all]
            [rapids.objects.stack-frame :as sf])
  (:import (clojure.lang ArityException)))

;;;; Partitioner

;;;
;;; Breaks flow bodies into partitions representing sequences of expressions
;;; which can be executed without being suspending.
;;;
;;; Partitions are the AST representations of code, used to define "partition functions" .
;;; Breaks are introduced by calls to flows or to any suspending function like `listen!`.
;;; Suspending functions can be defined by tagging the function ^:suspending. It tells
;;; the partitions to introduce a break because calling the function _might_ return
;;; a suspending event.
;;;
;;; When a flow is called, a StackFrame is created which captures the bindings
;;; and the address where execution should resume. When a `(listen! :permit "foo")`
;;; expression is encountered, the stack is persisted to a non-volatile storage
;;; and associated with these values. When an event matching the current `*run-id*`
;;; and `permit` is received, the run (which holds the stack) is retrieved, and
;;; execution is resumed at the point in the flow after the `(listen!...)` call,
;;; with the bindings that were present when `(listen!...)` was invoked. At the end
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
  partition-functional-expr partition-bindings
  partition-if-expr partition-let*-expr partition-fn*-expr
  partition-do-expr partition-loop*-expr partition-special-expr partition-recur-expr
  partition-vector-expr partition-map-expr
  partition-suspend-expr partition-flow-expr partition-callcc-expr
  resume-at)

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
   {:pre  [(vector? params)]
    :post [(vector (first %))]}
   (let [dirty-address partition-address]
     (loop [iter-body body
            pset (pset/create)
            cur-address (a/child address 0)
            partition-address partition-address
            part-body []
            start-body nil
            any-suspend? false]
       (if-not (empty? iter-body)
         (let [[expr & rest-body] iter-body
               [pexpr expr-pset suspend?] (with-tail-position [(empty? rest-body)]
                                            (partition-expr expr partition-address cur-address params))
               pset (pset/combine pset expr-pset)
               next-address (a/increment cur-address)]
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
             [part-body, (pset/remove-unforced pset), false])))))))

;;
;; Expressions
;;

(defn partition-expr
  "Breaks an expression down into partitions - blocks of code which are wrapped into
  functions which perform incremental pieces of evaluation.

  Args:
    expr - the expression to be partitioned
    partition = Address of the partition currently being built
    address - Address of the expression
    params - vector of currently bound parameters at this address point
            these become keys when defining functions
    data-key - where the code should

  Returns:
  [
    start       - an s-expression which starts computation of expr
    pset        - a map of addresses to function definitions
    suspend?    - boolean indicating whether this expression could suspend the flow
  ]"
  [expr partition-addr address params]
  {:pre [(vector? params)]}
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (seq? mexpr) (partition-list-expr expr mexpr partition-addr address params)
      (vector? mexpr) (partition-vector-expr expr partition-addr address params)
      (map? mexpr) (partition-map-expr expr partition-addr address params)
      :otherwise [expr, nil, nil])))

(defn special-form-op? [x] (#{'if 'do 'let* 'fn* 'loop* 'quote 'recur} x))
(defn flow-op? [x] (#{'flow 'rapids/flow} x))
(defn callcc-op? [x] (#{'callcc `rapids/callcc} x))

(defn partition-list-expr
  [expr mexpr partition-addr address params]
  {:pre [(vector? params)]}
  (let [op (first expr)
        mop (first mexpr)
        ops [op mop]]
    (cond
      ;; attempt to detect operator in expression
      (some special-form-op? ops) (partition-special-expr mop expr mexpr partition-addr address params)
      ;; note: the following are not special-symbols, though the docs list them as special forms:
      (some callcc-op? ops) (partition-callcc-expr expr mexpr partition-addr address params)
      (some suspending-operator? ops) (partition-suspend-expr expr mexpr partition-addr address params)
      (some flow/flow-symbol? ops) (partition-flow-invokation-expr mop expr mexpr partition-addr address params)
      (some flow-op? ops) (partition-flow-expr expr mexpr partition-addr address params)
      :else (partition-fncall-expr mop expr mexpr partition-addr address params))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr mexpr partition-addr address params]
  (case op
    if (partition-if-expr expr mexpr partition-addr address params)
    do (partition-do-expr expr mexpr partition-addr address params)
    let* (partition-let*-expr expr mexpr partition-addr address params)
    fn* (partition-fn*-expr expr mexpr partition-addr address params)
    loop* (partition-loop*-expr expr mexpr address params)
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
  (letfn [(check-non-suspending [sig]
            (let [body (rest sig)
                  [_, _, suspend?] (partition-body body partition-addr address params)]
              (if suspend?
                (throw-partition-error "Illegal attempt to suspend in function body" expr))
              suspend?))]
    (let [[_, sigs] (closure/extract-fn-defs mexpr)
          _ (doseq [sig sigs] (check-non-suspending sig))
          [ctor, pset] (closure/closure-constructor mexpr address params)]

      [ctor, pset, false])))

(defn partition-flow-expr
  "Handles anonymous flow definition such as (flow [...] ...).
  Akin to Clojure's (fn [...] ...), but the body may contain suspending expressions.
  The partitioner partitions the flow body and returns a closure which invokes an
  entry point function for the flow."
  [expr, mexpr, partition-addr, address params]
  (let [[entry-fn-def, pset] (partition-flow-body (meta expr) address (rest expr) params)
        entry-address (a/child address 'entry-point)
        pset (pset/add pset entry-address params [entry-fn-def])
        start-expr `(->Closure ~entry-address ~(bindings-expr-from-params params) true)]
    ;; TODO: fix the suspend? return value
    ;;       The third return value `suspend?` should be false since the (->Closure ...) form doesn't suspend
    ;;       However, if we do this, the rest of the partitioner code will ignore
    ;;       pset and start-expr. As a result, anonymous flows produce a little bit of
    ;;       ugly overhead.
    [start-expr, pset, true]))

(defn partition-let*-expr
  [_, mexpr, partition-addr, address, params]
  (let [address (a/child address 'let)
        binding-address (a/child address 0)
        body-address (a/child address 1)
        [_ bindings & body] mexpr
        [keys, args] (map vec (reverse-interleave bindings 2))

        [body-start, body-pset, body-suspend?]
        (partition-body body, partition-addr, body-address, (vec (concat params keys)))

        [bind-start, bind-pset, bind-suspend?]
        (partition-bindings keys, args, partition-addr, binding-address, params, body-start)

        pset (pset/combine body-pset bind-pset)]
    [bind-start, pset, (or bind-suspend? body-suspend?)]))

(defn partition-if-expr
  [expr mexpr partition-addr address params]
  (let [[_ test then else] mexpr
        address (a/child address 'if)
        [test-addr, then-addr, else-addr] (map #(a/child address %) [0 1 2])

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
  [_ mexpr partition address params]
  (let [[op & body] mexpr
        address (a/child address 'do)
        [bstart, pset, suspend?] (partition-body body partition address params)]
    (assert (= op 'do))
    [`(do ~@bstart), pset, suspend?]))

(defn partition-loop*-expr
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
  ;;                    around (flow/exec <loop-partition/1> ...) where
  ;;                    the bindings contain the updated loop variables
  [expr mexpr address params]
  (let [op (first expr)
        [_ bindings & loop-body] (if (in? '[loop loop*] op) expr mexpr)
        form-address (a/child address 'loop)
        binding-partition (a/child form-address 0)
        loop-partition (a/child form-address 1)

        [loop-params, loop-initializers] (reverse-interleave bindings 2)]

    ;; we need to partition the loop body FIRST, then pass the start-body to partition-bindings
    (let [loop-body-params (-> (concat params loop-params) distinct vec)

          ;; partition the loop body with the loop-partition registered as a recur
          ;; binding point; if recur calls happen within this partition, they
          ;; are treated as regular recur calls; otherwise, the recur is being executed
          ;; in a resumed runlet, so a call to the loop-partition is generated.
          [start-body, body-pset, body-suspend?]
          (with-tail-position [:reset]
            (with-binding-point [loop-partition loop-params loop-body-params] ; partition-recur-expr will use this
              (partition-body loop-body loop-partition loop-partition loop-body-params)))

          ;; the loop merely rebinds the loop parameters provided by the partition
          ;; note: we need a normal (loop ...) expression because we may have normal non-suspending
          ;; recur expressions in addition to suspending recur expressions.
          loop-partition-body [`(loop [~@(apply concat (map #(vector % %) loop-params))] ~@start-body)]
          loop-partition-params (vec (concat loop-params params))
          loop-pset (pset/add (pset/create) loop-partition loop-partition-params loop-partition-body)

          flow-continue-bindings (bindings-expr-from-params loop-partition-params)
          binding-body `[(flow/call-partition ~loop-partition ~flow-continue-bindings)]

          ;; now we can compute the initializers and give it the partitioned loop start...
          [binding-start, bindings-pset, binding-suspend?]
          (partition-bindings loop-params, loop-initializers, binding-partition, binding-partition, params, binding-body)

          any-suspend? (or binding-suspend? body-suspend?)

          pset (pset/combine body-pset bindings-pset loop-pset)]
      (if any-suspend?
        [binding-start, pset, any-suspend?]
        [expr, pset, false]))))

(defn partition-recur-expr
  "Returns:
  [start, pset, suspend?]"
  [expr, mexpr, partition-address, address, params]
  (letfn [(make-call [args]
            (let [partition-params (:partition-params *recur-binding-point*)
                  loop-params (:loop-params *recur-binding-point*)
                  recur-arity (count loop-params)
                  bindings `(merge
                              ~(bindings-expr-from-params partition-params) ;; ensure all the partition params are represented
                              ~(bindings-expr-from-params loop-params args))]
              (if-not (= (count args) recur-arity)
                (throw-partition-error "Mismatched argument count to recur" expr "expected: %s args, got: %s"
                  (count args) recur-arity))
              `(flow/call-partition ~(:address *recur-binding-point*)
                 ~bindings)))]
    (if-not *recur-binding-point*
      (throw-partition-error "No binding point" expr))
    (if-not *tail-position*
      (throw-partition-error "Can only recur from tail position" expr))

    (let [[op & args] mexpr
          loop-address (:address *recur-binding-point*)

          address (a/child address 'recur)

          [start, pset, suspend?]
          (partition-functional-expr op, expr, mexpr, partition-address, address, params, make-call)

          same-partition (and (false? suspend?) (= partition loop-address))]
      (if same-partition
        [start, pset, false]                                ; plain vanilla recur!
        (if suspend?
          [start, pset, true]                               ;; arguments suspended
          (if same-partition
            [expr, pset, false]
            [(make-call args), pset, false]))))))

;;
;;
;;
(defn partition-suspend-expr
  [expr, mexpr, partition-addr, address, params]
  (let [op (first mexpr)
        [start, pset, _]
        (partition-functional-expr op expr mexpr partition-addr address params
          #(cons op %))]
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
  {:pre [(vector? params)]}
  (letfn [(call-form [args]
            (with-meta (make-call-form args) (meta expr)))]
    (let [address (a/child address op)
          [_ & args] mexpr
          keys (nsymbols address (count args))
          pcall-body [(call-form keys)]
          [start, pset, suspend?]
          (partition-bindings keys args partition-address address params
            pcall-body)]
      (if suspend?
        [start, pset, true]                                 ; partition bindings has provided the correct result
        [(call-form args), nil, false]))))

(defn partition-map-expr
  [expr partition-addr address params]
  {:pre [(map? expr)
         (a/address? partition-addr)
         (a/address? address)
         (vector? params)]}
  ;; TODO: this could be simplified...
  (let [fake-op (symbol "_map_")
        pseudo-expr (cons fake-op (apply concat (map identity expr)))]
    (partition-functional-expr fake-op pseudo-expr pseudo-expr partition-addr address params
      (fn [args] (into {} (map vec (partition 2 args)))))))

(defn partition-vector-expr
  [expr partition-addr address params]
  (let [fake-op (symbol "_vec_")
        expr-with-op (with-meta `(~fake-op ~@expr) (meta expr))]
    (partition-functional-expr fake-op expr-with-op expr-with-op partition-addr address params
      #(vec %))))

(defn partition-callcc-expr
  [exp, _, partition-addr, address, params]
  (let [[op & args] exp
        _ (assert (callcc-op? op))
        [f & _] args
        faddr (a/child address 'callcc 0)
        [fstart, fpset, _] (partition-expr f, partition-addr, faddr, params)]
    (if (not= 1 (count args))
      (throw (ArityException. 2 "rapids/callcc")))
    [`(let [stack# (rapids.runtime.runlet/current-run :stack)
            fstart# ~fstart
            continuation# (rapids.language.operators/fcall rapids.language.continuation/make-current-continuation stack#)]
        (rapids/fcall fstart# continuation#)), fpset, true]))

(defn partition-flow-invokation-expr
  [op, expr, mexpr, partition-addr, address, params]
  (let [[start, pset, _] (partition-functional-expr op expr mexpr partition-addr address params
                           (fn [args]
                             `(rapids.language.operators/fcall ~op ~@args)))]
    [start, pset, true]))

(defn partition-fncall-expr
  [op, expr, mexpr, partition-addr, address, params]
  {:pre [(vector? params)]}
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
  {:pre [(a/address? partition-address)
         (a/address? address)
         (vector? params)
         (vector? body)]}
  (with-tail-position [false]
    (let [dirty-address partition-address]
      (loop
        [[key & rest-keys] keys
         [arg & rest-args] args
         current-bindings []                                ; the new bindings introduced to the partition
         partition-address partition-address
         arg-address (a/child address 0)                    ; address of the arg
         params params                                      ; params provided to this partition
         start nil
         any-suspend? false
         pset (pset/create)]
        (if key
          (let [[arg-start, arg-pset, suspend?]
                (partition-expr arg, partition-address, arg-address, params) ;

                pset (pset/combine pset arg-pset)
                next-address (a/increment arg-address)
                new-params (conj params key)]
            (if suspend?
              (let [resume-pexpr `(resume-at [~next-address [~@params] ~key]
                                    ~arg-start)
                    let-bindings (vec (apply concat current-bindings))
                    pexpr (if (> (count current-bindings) 0)
                            `(let [~@let-bindings] ~resume-pexpr)
                            resume-pexpr)
                    pset (pset/add pset partition-address params [pexpr])
                    start (or start pexpr)]
                (recur rest-keys, rest-args, [], next-address, next-address,
                  new-params, start, (or suspend? any-suspend?), pset))
              (do
                (recur rest-keys, rest-args
                  (conj current-bindings [key arg-start]),
                  partition-address, next-address,
                  new-params, start, any-suspend?, pset))))

          ;; finalize the last partition by executing the body with the
          ;; bound params
          (let [let-bindings (vec (apply concat current-bindings))
                final-body (if (> (count current-bindings) 0) [`(let [~@let-bindings] ~@body)] (vec body))
                clean-pset (pset/delete pset dirty-address)
                start (or start (first final-body))
                pset (pset/add clean-pset partition-address params final-body)]
            [start, pset, any-suspend?]))))))

;;;
;;; Partition flow body
;;;

(declare params-from-args extract-signatures partition-signature)

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
  [entry-fn-def, pset]

  entry-fn-def - a list of the form (fn ([...] ...) ([...] ..) ...)"
  ([m address fdecl]
   (partition-flow-body m address fdecl []))

  ([m address fdecl params]
   (let [sigs (extract-signatures fdecl)
         entry-point-name (str (a/to-string address) "__entry-point")
         [psets, sig-defs] (reverse-interleave
                             (apply concat
                               (map-indexed
                                 (fn [idx sig]
                                   (partition-signature m (a/child address idx) sig params))
                                 sigs))
                             2)
         pset (apply pset/combine psets)
         entry-fn-def `(fn ~(symbol entry-point-name) ~@sig-defs)]
     [entry-fn-def, pset])))

(defn- partition-signature
  "Returns a pset and and a partitioned sig definition.

  For example, this function definition has 2 signatures aka
  E.g., (defn foo
          ([] (foo :bar))
          ([msg] (println \"foo\" msg))"
  [m address sig params]
  (let [[args & code] sig
        params (vec (concat (params-from-args args [] (-> m :line)) params))
        [start-body, pset, _] (partition-body (vec code) address address params)
        pset (pset/add pset address params start-body)
        entry-bindings (bindings-expr-from-params params)]
    [pset, `([~@args] (flow/call-partition ~address ~entry-bindings))]))

;;
;; HELPERS
;;

(defn- params-from-args
  "given an argument vector, returns a vector of symbols"
  [args params line]
  (let [arg (first args)]
    (cond
      (= arg '&) (recur (rest args) params line)
      (map? arg) (recur (rest args) (concat params (:keys arg)) line)
      (symbol? arg) (recur (rest args) (conj params arg) line)
      (nil? arg) (vec params)
      :else (throw (ex-info (str "Unexpected argument " arg)
                     {:type :compiler-error})))))

(defn- extract-signatures [fdecl]
  (let [[_ _ & sigs] (macroexpand `(fn random-name# ~@fdecl))]
    sigs))

;;
;; resume-at
;;
(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the partition
  params - list of parameters needed by the partition
  data-key - the key to which the value of form will be bound in the partition
  body - expression which may suspend the run

  Returns:
  value of body"
  ([[address params data-key] & body]
   (:pre [(a/address? address)
          (vector? params)
          (or (nil? data-key) (symbol? data-key))])
   `(let [bindings# ~(bindings-expr-from-params params)]
      (rapids.runtime.runlet/push-stack! ~address bindings# '~data-key)
      ~@body)))
