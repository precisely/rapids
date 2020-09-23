(ns longterm.partitioner
  (:require [longterm.address :as address]
            [longterm.continuation_set :as cset])
  (:use longterm.thunk))

(declare partition-body partition-expr partition-bindings partition-fncall-expr

  partition-list-expr partition-flow-expr
  partition-if-expr partition-let-expr partition-fn-expr
  partition-do-expr partition-loop-expr partition-special-expr partition-recur-expr
  partition-vector-expr partition-map-expr macroexpand-keeping-metadata
  extract-bindings md5 md5-hash)

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
  [body continuations next-address result-key]
  (list body continuations next-address))

(defn nsymbols
  ([n] (nsymbols n 0))

  ([n start]
   (map #(gensym (str "arg" %)) (range start n))))

; (defn partition-expr ;;; => [partition0-body continuations flow-breaker? value-form]
; (defn partition-fn-args ;; => [partition0-body continuations flow-breaker? value-forms]

(declare a b c d e f g e f g h i j k l m n o p q r s t u v w x y z fl1--- fl2--- fl3--- fl4--- fl5--- deflow
  fl4-arg0 resume-at arg0 arg1 if-arg0 start say wait-for)



(defn partition-expr
  "Breaks an expression down into partitions - blocks of code which are wrapped into
  continuation functions which perform incremental pieces of evaluation.

  expr - the expression to be partitioned
  address - Address of the expression
  address-next - the next execution point after expr is complete
  result-key - binding key which hints where to store the result of expr
               this is passed to continue-with to record where the eventual result
               of expr should be bound
  params -  vector of currently bound parameters at this address point
            these become keys when defining continuations

  Returns:
  [
    start - code representing the first partition
    cset - a map of address points to function definitions
    params - params required by the continuation that contains value-form
    value-form - either the expression itself or a symbol which contains the value of the expression
  ]"
  ([expr address params]
   (let [mexpr (macroexpand-keeping-metadata expr)]
     (cond
       (list? mexpr) (partition-list-expr expr mexpr address params)
       (vector? mexpr) (partition-vector-expr params expr mexpr)
       (map? mexpr) (partition-map-expr params expr address)
       :otherwise expr))))

(defn partition-list-expr
  [expr mexpr address params]
  (let [op (first mexpr)]
    (cond
      (fn? op) (partition-fn-expr expr mexpr address params)
      (special-symbol? op) (partition-special-expr expr mexpr address params))))

(defn partition-fncall-expr
  [expr, mexpr, address, params]
  (let [[op, args] expr
        keys        (nsymbols (count args))
        address     (address/child address op)

        [start, cset, params, binding-values]
        (partition-bindings keys args address params)

        fncall-expr (vary-meta `(op ~@binding-values) merge (meta expr))
        [start, cset, params, fncall-expr]
    ))

(defn constant?
  "True if x represents a constant value"
  [x]
  (or (string? x) (keyword? x) (number? x) (char? x)
    (contains? [true false nil ##Inf ##-Inf ##NaN] x)
    (and (list? x) (= (first x) 'quote))))

(defn add-arg-partition
  [cset bindings arg-start arg-cset address params result-key]
  (letfn [(non-constant-bindings [b] (remove #(constant? (second %)) b))]
    (let [binding-params (map #(first %) (non-constant-bindings bindings))
          next-params    (conj (concat params binding-params) result-key)
          body           (if (> (count bindings) 0)
                           `((let ~bindings ~@arg-start))
                           arg-start)
          next-cset      (-> cset (cset/continue address next-params body)
                           (cset/combine arg-cset))]
      [next-cset next-params])))

(defn partition-bindings
  "Takes a list of arguments to a functions and partitions them, breaking on flow / wait-for calls.
  Args:
  keys - the keys to which values should be bound
  args - vector of forms passed to the function
  address - the lexical parent address (used to generate names for continuations required for arg eval)
  next-address - points to the continuation which will receive the argument bindings or forms
  params - parameters available in the lexical context when args are evaled
  fn-bindings - if true, bindings are eliminated when possible (scalars and terminal non-flow-breaking forms)

  Returns:
  [start - vector of forms or nil (nil indicates we have normal, non-flow-breaking args)
   cset - ContinuationSet containing all the continuations needed
   params - parameters for the final continuation (provide this to cset-add)
   binding-values] - function can be applied to these args within the final partition

  E.g.,
  (deflow asdf [z]
    (myfn                 |- <asdf:0> (partition 3)
      (a)      ; arg0     |- <asdf:0/myfn/0> (partition0)
      (flow1)  ; arg1     |
      (b)      ; arg2     |- <asdf:0/myfn/2> (partition1)
      (flow2)  ; arg3     |
      (c)))    ; (c)      |- <asdf:0> (parition2)

   (fn [{:keys [z arg0 arg1 arg2 arg3]}]
      (myfn arg0 arg1 arg2 arg3 (c)))

      (deflow f1 [z] (if z (do (c) (d) (f2)) nil))
      (deflow f2 [] (a) (b) (f3))
      (deflow f3 [] (e) (f) (wait-for-event :user-input)) => +DELAY+

  On input, next-address = asdf:0
  1. compute argument-body and params:
     argument body: [arg1 arg2 arg3 arg4 (c)]
     parameters for continuation: [z arg1 arg3 arg3 arg4]

  2. compute continuations:
     {
       <asdf:0/0> (continuation-fn-def '[z arg0] '(let [arg1 (a)] (flow1)) <asdf:0/2> 'arg2)
       <asdf:0/2> (continuation-fn-def '[z arg0 arg1] '(let [arg3 (b)] (flow2)) <asdf:0> 'arg4)
     }
  3. return:
     [
      [`(let [arg0 (a)] ~(continue-to <asdf:0/myfn/0> [z] (flow1))]
      [arg1 arg2 arg3 arg4 (c)] ; fn argument list
      [z arg1 arg2 arg3 arg4]   ; parameters required for constructing <asdf:0> continuation
     ]

  Note that the entry point of asdf will be something like
  ```
  (let [arg0 (a)]
    (thunk-to-<asdf:0/myfn/2> ; pseudocode
      (flow1)))
  ```
  And the calling function will create a continuation:
  {
    <asdf:0> (fn [z arg0 arg1 arg2 arg3 arg4] (myfn arg0 arg1 arg2 arg3 arg4 (c)))
  }"
  [keys args address start-params]
  (loop [[key & rest-keys] keys
         [arg & rest-args] args
         cur-part-address (address/child address 0)
         params           start-params
         bindings         []            ; new bindings created in this partition
         fn-args          []
         cset             (cset/create)
         start            nil]
    (let [[arg-start arg-cset value-form] (partition-expr arg address params key)

          is-flow-breaker? (-> arg-start not nil?)]
      (if is-flow-breaker?
        (let [next-part-address (address/increment cur-part-address)
              fn-args           [conj fn-args key]
              [next-cset next-params] (add-arg-partition cset bindings arg-start arg-cset address params key)
              start             (or start partition-body)]
          (if rest-args
            (recur rest-keys rest-args next-part-address next-params [] fn-args next-cset start)
            [start cset next-params fn-args]))
        ;; normal function argument
        (let [[fn-args bindings] (if (constant? value-form)
                                   [fn-args (conj binding [key arg])]
                                   [(conj fn-args value-form) bindings])]
          (if rest-args
            (recur rest-keys rest-args cur-part-address params bindings fn-args cset start)
            [start cset params fn-args])))))) )

(defn macroexpand-keeping-metadata
  [expr]
  (vary-meta (macroexpand expr) merge (meta expr)))

(defn partition-body
  ([body                                ; list of expressions which should be partitiond
    address                             ; address of the current body; body forms will be address/0, address/1 etc
    start-params                        ; vector of symbols which will be bound on entry
    continuations                       ; map of address-points to continuations forms (unpartitiond code)
    next-address                        ; continue to this address after final form
    result-key                          ; if provided, final form should return result to this binding key (using with-thunk)
    point]                              ; point to start from
   "Returns [start-body continuation-code-map flow-breaker?]"
   (loop [body          body
          continuations continuations
          point         point
          flow-break?   nil
          body-fragment ()]
     (let [[expr & next-body] body
           expr-address      (address/child address point)

           ;; the next-address is the address of the next form in this body
           ;; unless we're at the end, in which case it is next-address (the point after `body`)
           expr-next-address (if next-body
                               (address/child address (inc point))
                               next-address)
           [e-form continuations e-break?] (partition-expr params expr continuations expr-address expr-next-address
                                             ;; if this is the last form, it should pass its value to result-key
                                             (if-not next-body result-key))]
       (if e-break?
         ; if e-form is a flow-breaker
         (let [body-fragment `(~@body-fragment ~(thunk/continue-to [expr-next-address params result-key] e-form))]
           (if (= point 0)
             ; body fragment may be incorporated into another form,
             ; don't create a continuation just yet... just get the remaining continuations
             (let [[_ continuations _] (partition-body params next-body continuations address next-address result-key (inc point))]
               [body-fragment continuations true])

             ; body fragment is guaranteed to be needed as a continuation
             ; so create one
             (let [continuations (assoc continuations address (thunk/make-continuation-form params body-fragment))]
               [result-key continuations true])))

         ; e-form is not a flow-breaker - merely add the form to the body fragment and continue
         (if next-body
           ; if there are more forms, keep adding to the body-fragment
           (recur next-body continuations (inc point) flow-break? `(~@body-fragment ~e-form))

           ; we're done - return the result
           [body-fragment continuations flow-break?]))))))


(defn bindings-from-args
  "given an argument vector, "
  ([args] (bindings-from-args args []))
  ([args bindings]
   (let [arg (first args)]
     (cond
       (= arg '&) (bindings-from-args (rest args) bindings)
       (map? arg) (bindings-from-args (rest args) (concat bindings (:keys args)))
       (= arg nil) bindings
       :else (throw (str "Unexpected argument " arg))))))
