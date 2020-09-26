(ns longterm.partition
  (:require [longterm.address :as address]
            [longterm.util :refer [refers-to?]]
            [longterm.continuation_set :as cset])
  (:use longterm.stack))

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

(defn nsymbols
  ([n] (nsymbols n 0))

  ([n start]
   (map #(gensym (str "arg" %)) (range start n))))

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
    start - code representing the first partition
    cset - a map of addresses to function definitions
    suspending? - boolean
  ]"
  [expr address params]
  (let [mexpr (macroexpand-keeping-metadata expr)]
    (cond
      (list? mexpr) (partition-list-expr expr mexpr address params)
      (vector? mexpr) (partition-vector-expr params expr mexpr)
      (map? mexpr) (partition-map-expr params expr address)
      :otherwise expr)))

(defn partition-list-expr
  [expr mexpr address params]
  (let [op (first mexpr)]
    (cond
      (refers-to? fn? op) (partition-fncall-expr expr mexpr address params)
      (special-symbol? op) (partition-special-expr expr mexpr address params))))

(defn partition-fncall-expr
  [expr, mexpr, address, params]
  (let [[op, args] mexpr
        keys (nsymbols (count args))

        [start, cset, params, dest-bindings, dest-address, suspend?]
        (partition-bindings keys args address params)]
    (if suspend?
      (let [fncall-expr (vary-meta `(let ~dest-bindings (~op ~@args)) merge (meta expr))
            cset        (cset/add cset, dest-address, params, fncall-expr)]
        [start, cset, suspend?])
      [expr, nil, nil])))

(defn partition-bindings
  [keys, args, address, params]
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

      ;; return result
      [start, cset, params, cur-part-bindings, cur-part-address, any-suspend?])))

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
      (let [[pexpr expr-cset suspend?] (partition-expr expr part-address start-params)
            cset (cset/combine cset expr-cset)]
        (if suspend?
          (let [next-address (address/increment address)
                part-body    (conj part-body `(resume-at [~next-address ~params] ~@pexpr))
                cset         (cset/add cset part-address params part-body)]
            (recur rest-body cset next-address [] (or start-body part-body) true))
          (recur rest-body cset part-address part-body start-body any-suspend?)))
      ;; return result
      [start-body, cset, any-suspend?])))

(defn partition-let-expr
  [expr mexpr address params]
  (let [bindings    (second mexpr)
        keys        (map first bindings)
        vexprs      (map second bindings)
        body        (nthrest mexpr 2)
        address     (address/child address 'let)

        [bindings-start, bindings-cset, params, body-bindings, body-address, bindings-suspend?]
        (partition-bindings keys vexprs address params)

        body-params (concat params (map first bindings))
        [body-start, body-cset, body-suspend?]
        (partition-body body body-address body-params)

        cset        (cset/combine bindings-cset body-cset)]
    (if bindings-suspend?
      ()
      ())))

;; HELPERS
(defn macroexpand-keeping-metadata
  [expr]
  (vary-meta (macroexpand expr) merge (meta expr)))

;(defn add-arg-partition
;  [cset bindings arg-start arg-cset address params result-key]
;  (letfn [(non-constant-bindings [b] (remove #(constant? (second %)) b))]
;    (let [binding-params (map #(first %) (non-constant-bindings bindings))
;          next-params    (conj (concat params binding-params) result-key)
;          raw-body       (if (> (count bindings) 0)
;                           `((let ~bindings ~@arg-start))
;                           arg-start)
;          next-cset      (-> cset (cset/continue address next-params body)
;                           (cset/combine arg-cset))]
;      [next-cset next-params])))
;
;(defn partition-bindings
;  "Takes a list of arguments to a functions and partitions them, breaking on flow / wait-for calls.
;  Args:
;  keys - the keys to which values should be bound
;  args - vector of forms passed to the function
;  address - the lexical parent address (used to generate names for continuations required for arg eval)
;  params - parameters available in the lexical context when args are evaled
;  fn-bindings - if true, bindings are eliminated when possible (e.g., literals)
;
;  Returns:
;  [
;    start - vector of forms or nil (nil indicates we have normal, non-might-suspend?ing args)
;    cset - ContinuationSet containing all the continuations needed
;    params - parameters for the final continuation (provide this to cset-add)
;    fn-args -
;    let-bindings -
;  ]
;
;  E.g.,
;  (deflow asdf [z]
;    (myfn                 |- <asdf:0> (partition 3)
;      (a)      ; arg0     |- <asdf:0/myfn/0> (partition0)
;      (flow1)  ; arg1     |
;      (b)      ; arg2     |- <asdf:0/myfn/2> (partition1)
;      (flow2)  ; arg3     |
;      (c)))    ; (c)      |- <asdf:0> (parition2)
;
;   (fn [{:keys [z arg0 arg1 arg2 arg3]}]
;      (myfn arg0 arg1 arg2 arg3 (c)))
;
;      (deflow f1 [z] (if z (do (c) (d) (f2)) nil))
;      (deflow f2 [] (a) (b) (f3))
;      (deflow f3 [] (e) (f) (wait-for-event :user-input)) => +DELAY+
;
;  On input, next-address = asdf:0
;  1. compute argument-body and params:
;     argument body: [arg1 arg2 arg3 arg4 (c)]
;     parameters for continuation: [z arg1 arg3 arg3 arg4]
;
;  2. compute continuations:
;     {
;       <asdf:0/0> (continuation-fn-def '[z arg0] '(let [arg1 (a)] (flow1)) <asdf:0/2> 'arg2)
;       <asdf:0/2> (continuation-fn-def '[z arg0 arg1] '(let [arg3 (b)] (flow2)) <asdf:0> 'arg4)
;     }
;  3. return:
;     [
;      [`(let [arg0 (a)] ~(continue-to <asdf:0/myfn/0> [z] (flow1))]
;      [arg1 arg2 arg3 arg4 (c)] ; fn argument list
;      [z arg1 arg2 arg3 arg4]   ; parameters required for constructing <asdf:0> continuation
;     ]
;
;  Note that the entry point of asdf will be something like
;  ```
;  (let [arg0 (a)]
;    (thunk-to-<asdf:0/myfn/2> ; pseudocode
;      (flow1)))
;  ```
;  And the calling function will create a continuation:
;  {
;    <asdf:0> (fn [z arg0 arg1 arg2 arg3 arg4] (myfn arg0 arg1 arg2 arg3 arg4 (c)))
;  }"
;  [keys args address start-params]
;  (loop [[key & rest-keys] keys
;         [arg & rest-args] args
;         cur-part-address (address/child address 0)
;         params           start-params
;         bindings         []            ; new bindings created in this partition
;         fn-args          []
;         cset             {}
;         start            nil]
;    (let [[arg-start arg-cset value-form] (partition-expr arg address params)
;
;          might-suspend?? (-> arg-start not nil?)]
;      (if might-suspend?
;        (let [next-part-address (address/increment cur-part-address)
;              fn-args           [conj fn-args key]
;              [next-cset next-params] (add-arg-partition cset bindings arg-start arg-cset address params key)
;              start             (or start partition-body)]
;          (if rest-args
;            (recur rest-keys rest-args next-part-address next-params [] fn-args next-cset start)
;            [start cset next-params fn-args]))
;        ;; normal function argument
;        (let [[fn-args bindings] (if (constant? value-form)
;                                   [fn-args (conj binding [key arg])]
;                                   [(conj fn-args value-form) bindings])]
;          (if rest-args
;            (recur rest-keys rest-args cur-part-address params bindings fn-args cset start)
;            [start cset params fn-args])))))) )
;
;(defn constant?
;  "True if x represents a constant value"
;  [x]
;  (or (string? x) (keyword? x) (number? x) (char? x)
;    (contains? [true false nil ##Inf ##-Inf ##NaN] x)
;    (and (list? x) (= (first x) 'quote))))