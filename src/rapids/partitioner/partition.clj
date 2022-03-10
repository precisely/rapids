(ns rapids.partitioner.partition
  (:require [rapids.objects.address :as a]
            [rapids.objects.closure :refer [->Closure]]
            [rapids.objects.flow :as flow]
            [rapids.objects.signals :refer [suspending-operator?]]
            [rapids.partitioner.closure :as closure]
            [rapids.partitioner.partition-set :as pset]
            [rapids.partitioner.partition-utils :refer :all]
            [rapids.partitioner.recur :refer [*recur-binding-point* *tail-position* with-binding-point with-tail-position]]
            [rapids.support.debug :refer :all]
            [rapids.support.util :refer :all]
            [rapids.partitioner.resume-at :refer :all]
            [rapids.partitioner.macroexpand
             :refer [partition-macroexpand partition-macroexpand-1 exclude-from-gensym-replacement stable-symbol]])
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
  partition-functional-expr partition-bindings partition-bindings-new make-let-expr bindings-at-index
  partition-if-expr partition-let*-expr partition-fn*-expr
  partition-do-expr partition-loop*-expr partition-special-expr partition-recur-expr
  partition-vector-expr partition-map-expr partition-set-expr
  partition-java-interop-expr partition-java-new-expr
  partition-suspend-expr partition-flow-expr partition-callcc-expr
  partition-case-expr partition-case*-expr partition-binding-expr partition-set!-expr)

(defn default-partition-modifier [p _ _] p)
(defn partition-body
  "Partitions a list of expressions, e.g., for do, let and deflow forms
  Args:
   body     - list of expressions which should be partitiond
   partition - the current external partition address
   address  - address of the current body; body forms will be address/0, address/1 etc
   params   - vector of symbols which will be bound on entry
   modifier  - optional function which produces a modified version of the partition of the form (modifier p start? final?)
               where start? is true if p is the first partition, and final? is true if p is the last partition.
               Note that modifier will be called exactly once with both start? and final? set to true if the body
               contains a single partition.
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
   (partition-body body partition-address address params default-partition-modifier))
  ([body partition-address address params modifier]
   {:pre  [(vector? params)]
    :post [(vector (first %))]}
   (let [dirty-address partition-address]
     (loop [iter-body         body
            pset              (pset/create)
            cur-address       (a/child address 0)
            partition-address partition-address
            part-body         []
            start-body        nil
            any-suspend?      false]
       (if-not (empty? iter-body)
         (let [[expr & rest-body] iter-body
               [pexpr expr-pset suspend?] (with-tail-position (empty? rest-body)
                                            (partition-expr expr partition-address cur-address params))
               pset         (pset/combine pset expr-pset)
               next-address (a/increment cur-address)]
           (if suspend?
             (let [final-part-expr (if (> (count rest-body) 0) `(resume-at [~next-address [~@params] nil] ~pexpr) pexpr)
                   part-body       (modifier (conj part-body final-part-expr) (not start-body) (empty? rest-body))
                   pset            (pset/add pset partition-address params part-body)]

               ;; this partition ends here; next-address becomes the new partition-address
               (recur rest-body, pset, next-address, next-address, [], (or start-body part-body), suspend?))
             (recur rest-body, pset, next-address, partition-address, (conj part-body pexpr), start-body, any-suspend?)))

         ;; return result
         (let* [clean-pset (pset/delete pset dirty-address)
                part-body  (modifier part-body (not any-suspend?) true)
                pset       (if (> (count part-body) 0)
                             (pset/add clean-pset partition-address params part-body)
                             clean-pset)]
           (if any-suspend?
             [start-body, pset, any-suspend?]
             [part-body,      ; we haven't run make-partition-body on this code yet
              (pset/remove-unforced pset), false])))))))

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
    input-key - where the code should

  Returns:
  [
    start       - an s-expression which starts computation of expr
    pset        - a map of addresses to function definitions
    suspend?    - boolean indicating whether this expression could suspend the flow
  ]"
  [expr partition-addr address params]
  {:pre [(vector? params) (not (some constant? params))]}
  (letfn [(attempt-partitioning []
            (cond
              (non-partitioning? expr) [expr nil nil]
              (vector? expr) (partition-vector-expr expr partition-addr address params)
              (map? expr) (partition-map-expr expr partition-addr address params)
              (set? expr) (partition-set-expr expr partition-addr address params)
              (seq? expr) (partition-list-expr expr partition-addr address params)))]
    (binding [*partitioning-expr* (if (-> expr meta :line) expr *partitioning-expr*)]
      (or (attempt-partitioning)
        (let [expanded (partition-macroexpand-1 expr)
              expanded (if (and (seq? expanded) (instance? LazySeq expanded))
                         (seq expanded)
                         expanded)]
          (cond
            (non-partitioning? expanded) [expr, nil, nil]
            (identical? expanded expr)
            (throw-partition-error "Unhandled expression: %s" expr)
            :otherwise (let [m                       (meta expr)
                             expanded-meta           (update (or m {}) :source #(or (:source %) expr))
                             expanded-expr-with-meta (with-meta expanded expanded-meta)]
                         (partition-expr expanded-expr-with-meta partition-addr address params))))))))

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
  [expr partition-addr address params]
  {:pre [(vector? params)]}
  (let [op (first expr)]
    (cond
      ;; attempt to detect operator in expression
      ;; special handling for case expr.
      (case-op? op) (partition-case-expr expr partition-addr address params)
      (java-inter-op? op) (partition-java-interop-expr expr partition-addr address params)
      (java-new-op? op) (partition-java-new-expr expr partition-addr address params)
      (special-form-op? op) (partition-special-expr op expr partition-addr address params)
      (binding-op? op) (partition-binding-expr expr partition-addr address params)

      ;; note: the following are not special-symbols, though the docs list them as special forms:
      (callcc-op? op) (partition-callcc-expr expr partition-addr address params)
      (suspending-operator? op) (partition-suspend-expr expr partition-addr address params)
      (flow/flow-symbol? op) (partition-flow-invokation-expr op expr partition-addr address params)
      (flow-op? op) (partition-flow-expr expr partition-addr address params)
      (or (fn-like? op) (fn-op? op)) (partition-fncall-expr op expr partition-addr address params))))

;;
;; Special Symbols
;;
(defn partition-special-expr
  [op expr partition-addr address params]
  (case op
    if (partition-if-expr expr partition-addr address params)
    do (partition-do-expr expr partition-addr address params)
    let* (partition-let*-expr expr partition-addr address params)
    fn* (partition-fn*-expr expr partition-addr address params)
    loop* (partition-loop*-expr expr address params)
    case (partition-case-expr expr partition-addr address params)
    case* (partition-case*-expr expr partition-addr address params)
    quote [expr, nil, false]
    set! (partition-set!-expr expr partition-addr address params)
    recur (partition-recur-expr expr partition-addr address params)
    (throw-partition-error "Special operator not yet available in flows %s" op)))

(defn partition-fn*-expr
  "Throws if fn* contain suspending ops - this partitioner acts as a guard
  against knowing or inadvertent inclusion of a suspending operation inside a fn.
  This might happen inadvertently if the user uses a macro (like for) which
  expands into a (fn ...) expression. This is a bit of a blunt instrument,
  but not sure what else to do right now."
  [expr partition-addr address params]
  (letfn [(check-non-suspending [sig]
            (let [body (rest sig)
                  [_, _, suspend?] (partition-body body partition-addr address params)]
              (if suspend?
                (throw-partition-error "Illegal attempt to suspend in function body"))
              suspend?))]
    (let [[_, sigs] (closure/extract-fn-defs expr)
          _ (doseq [sig sigs] (check-non-suspending sig))
          [ctor, pset] (closure/closure-constructor expr address params)]
      ;; TODO: fix the suspend? return value
      ;;       The third return value `suspend?` should be false since the (->Closure ...) form doesn't suspend
      ;;       However, if we do this, the rest of the partitioner code will ignore
      ;;       pset and start-expr. As a result, anonymous flows produce a little bit of
      ;;       ugly overhead.

      [ctor, pset, true])))

(defn partition-flow-expr
  "Handles anonymous flow definition such as (flow [...] ...).
  Akin to Clojure's (fn [...] ...), but the body may contain suspending expressions.
  The partitioner partitions the flow body and returns a closure which invokes an
  entry point function for the flow."
  [expr, _, address params]
  (let [[entry-fn-def, pset] (partition-flow-body (meta expr) address (rest expr) params)
        m             (meta expr)
        entry-address (a/child address 'entry-point)
        pset          (pset/add pset entry-address params [entry-fn-def])
        start-expr    (with-meta `(->Closure ~entry-address ~(bindings-expr-from-params params) true) m)]
    ;; TODO: fix the suspend? return value
    ;;       The third return value `suspend?` should be false since the (->Closure ...) form doesn't suspend
    ;;       However, if we do this, the rest of the partitioner code will ignore
    ;;       pset and start-expr. As a result, anonymous flows produce a little bit of
    ;;       ugly overhead.
    [start-expr, pset, true]))

(defn partition-java-interop-expr [expr partition-addr address params]
  (let [source (-> expr meta :source)
        op     (first source)]
    (if source
      (partition-functional-expr op source partition-addr address params
        #(cons op (seq %)))
      (throw-partition-error "Java interop operator not yet supported"))))

(defn partition-java-new-expr [expr partition-addr address params]
  (let [[_ cls] expr
        op (symbol (str cls "."))]
    (partition-functional-expr op expr partition-addr address params
      #(list* 'new %))))

(defn partition-let*-expr
  ([expr, partition-addr, address, params]
   (partition-let*-expr expr, partition-addr, address, params default-partition-modifier))
  ([expr, partition-addr, address, params, modifier]
   (let [address         (a/child address 'let)
         binding-address (a/child address 0)
         body-address    (a/child address 1)
         [_ bindings & body] expr
         [keys, args] (map vec (reverse-interleave bindings 2))

         [body-start, body-pset, body-suspend?]
         (partition-body body, partition-addr, body-address, (vec (concat params keys)), modifier)

         [bind-start, bind-pset, bind-suspend?]
         (partition-bindings keys, args, partition-addr, binding-address, params, body-start)

         pset            (pset/combine body-pset bind-pset)]
     [bind-start, pset, (or bind-suspend? body-suspend?)])))

(defn partition-let*-expr-new
  ([expr, partition-addr, address, params]
   (partition-let*-expr expr, partition-addr, address, params default-partition-modifier))
  ([expr, partition-addr, address, params, modifier]
   (let [address         (a/child address 'let)
         binding-address (a/child address 0)
         body-address    (a/child address 1)
         [_ bindings & body] expr
         [keys, args] (map vec (reverse-interleave bindings 2))

         [bind-start, bind-pset, bindings-suspend-count, final-pset, final-binding-index, final-params, final-addr]
         (partition-bindings-new keys, args, true, partition-addr, binding-address, body-address, params)

         [body-start, body-pset, body-suspend?]
         (partition-body body, final-addr, body-address, final-params, modifier)

         bindings        (interleave (nthrest keys final-binding-index) (nthrest args final-binding-index))
         final-body      (make-let-expr bindings
                           (if body-suspend? body-start body))

         pset            (pset/add final-pset final-addr final-params final-body true)
         pset            (pset/combine pset bind-pset body-pset)]
     (let [suspends? (or (> bindings-suspend-count 0) body-suspend?)]
       (if suspends?
         [bind-start, pset, true]
         [expr, nil, false])))))

;;
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
(defn make-dynamic-binding-body-modifier
  "Bindings should be a vector of two-typles of the form [[*dynvar* val] ...]"
  [var-map]
  (fn [body start? _]
    {:pre [(vector? body)]}
    (if (-> var-map count (= 0))
      body
      (if start?
        ;; first partition: we need to establish the thread bindings.
        `[(rapids.runtime.runlet/enter-binding-body (fn [] ~@body), ~var-map, false)]

        ;; subsequent partitions: run dynamic bindings have been established, so the runtime
        ;; creates thread bindings automatically. All we need to do is undo the RDBs in
        ;; the final partition.
        body))))


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
  [expr, partition-addr, address, params]
  (let [address         (a/child address 'binding)
        binding-address (a/child address 0)
        body-address    (a/child address 1)
        [_ bindings & body] expr
        [dynvars, args] (map vec (reverse-interleave bindings 2))
        keys            (repeatedly (count dynvars) stable-symbol)
        body-bindings   (map #(vector %1 %2) dynvars keys)
        bindings-map    (into {} (map (fn [[lhs rhs]] `[(var ~lhs) ~rhs]) body-bindings))
        modifier        (make-dynamic-binding-body-modifier bindings-map)
        body            `[(rapids.runtime.runlet/push-run-bindings! ~bindings-map)
                          (let [result# (do ~@body)]
                            (rapids.runtime.runlet/pop-run-bindings!)
                            result#)]
        ;_ (println "bindings body= " body)
        [body-start, body-pset, body-suspend?]
        (partition-body body, partition-addr, body-address, (vec (concat params keys))
          modifier)

        [bind-start, bind-pset, bind-suspend?]
        (partition-bindings keys, args, partition-addr, binding-address, params, body-start)

        pset            (pset/combine body-pset bind-pset)]
    [bind-start, pset, (or bind-suspend? body-suspend?)]))

(defn partition-set!-expr [expr partition-addr address params]
  (let [[op lhs rhs & disallowed] expr
        isvar? (fn [o] (and (symbol? o) (var? (resolve o))))]
    (if-not (empty? disallowed) (throw (ArityException. 2 'set!)))
    (partition-functional-expr op `(set! :lhs ~rhs) partition-addr address params
      (fn [[_ rhs-param]]
        `(do
           ~@(if (isvar? lhs)
               `((rapids.runtime.runlet/set-run-dynamic-var (var ~lhs) ~rhs-param)))
           (set! ~lhs ~rhs-param))))))

(defn partition-if-expr
  [expr partition-addr address params]
  (let [[_ test then else] expr
        address     (a/child address 'if)
        [test-addr, then-addr, else-addr] (map #(a/child address %) [0 1 2])

        [test-start, test-pset, test-suspend?]
        (with-tail-position false
          (partition-expr test, partition-addr, test-addr, params))

        [then-start, then-pset, then-suspend?]
        (partition-expr then, partition-addr, then-addr, params)

        [else-start, else-pset, else-suspend?]
        (partition-expr else, partition-addr, else-addr, params)

        branch-addr address

        test-result (stable-symbol)
        start       (if test-suspend?
                      `(resume-at [~branch-addr, [~@params], ~test-result], ~test-start)
                      (with-meta `(if ~test ~then-start ~else-start) (meta expr)))
        branch-pset (if test-suspend?
                      (pset/add (pset/create) branch-addr `[~@params ~test-result]
                        `[(if ~test-result ~then-start ~else-start)]))
        full-pset   (pset/combine test-pset branch-pset then-pset else-pset)
        suspend?    (or test-suspend? then-suspend? else-suspend?)]
    [start, full-pset, suspend?]))

(defn partition-do-expr
  [expr partition address params]
  (let [[op & body] expr
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
  [expr address params]
  (let [op                (first expr)
        [_ bindings & loop-body] (if (in? '[loop loop*] op) expr)
        form-address      (a/child address 'loop)
        binding-partition (a/child form-address 0)
        loop-partition    (a/child form-address 1)

        [loop-params, loop-initializers] (reverse-interleave bindings 2)]

    ;; we need to partition the loop body FIRST, then pass the start-body to partition-bindings
    (let [loop-body-params       (-> (concat params loop-params) distinct vec)

          ;; partition the loop body with the loop-partition registered as a recur
          ;; binding point; if recur calls happen within this partition, they
          ;; are treated as regular recur calls; otherwise, the recur is being executed
          ;; in a resumed runlet, so a call to the loop-partition is generated.
          [start-body, body-pset, body-suspend?]
          (with-tail-position :reset
            (with-binding-point [loop-partition loop-params loop-body-params] ; partition-recur-expr will use this
              (partition-body loop-body loop-partition loop-partition loop-body-params)))

          ;; the loop merely rebinds the loop parameters provided by the partition
          ;; note: we need a normal (loop ...) expression because we may have normal non-suspending
          ;; recur expressions in addition to suspending recur expressions.
          loop-partition-body    [`(loop [~@(apply concat (map #(vector % %) loop-params))] ~@start-body)]
          loop-partition-params  (vec (concat loop-params params))
          loop-pset              (pset/add (pset/create) loop-partition loop-partition-params loop-partition-body)

          flow-continue-bindings (bindings-expr-from-params loop-partition-params)
          binding-body           `[(flow/call-partition ~loop-partition ~flow-continue-bindings)]

          ;; now we can compute the initializers and give it the partitioned loop start...
          [binding-start, bindings-pset, binding-suspend?]
          (partition-bindings loop-params, loop-initializers, binding-partition, binding-partition, params, binding-body)

          any-suspend?           (or binding-suspend? body-suspend?)

          pset                   (pset/combine body-pset bindings-pset loop-pset)]
      (if any-suspend?
        [binding-start, pset, any-suspend?]
        [expr, pset, false]))))

(defn partition-recur-expr
  "Returns:
  [start, pset, suspend?]"
  [expr, partition-address, address, params]
  (letfn [(make-call [args]
            (let [partition-params (:partition-params *recur-binding-point*)
                  loop-params      (:loop-params *recur-binding-point*)
                  recur-arity      (count loop-params)
                  bindings         `(merge
                                      ~(bindings-expr-from-params partition-params) ;; ensure all the partition params are represented
                                      ~(bindings-expr-from-params loop-params args))]
              (if-not (= (count args) recur-arity)
                (throw-partition-error "Mismatched argument count to recur. Expected: %s args, got: %s"
                  (count args) recur-arity))
              `(flow/call-partition ~(:address *recur-binding-point*)
                 ~bindings)))]
    (if-not *recur-binding-point*
      (throw-partition-error "No binding point"))
    (if-not *tail-position*
      (throw-partition-error "Can only recur from tail position"))

    (let [[op & args] expr
          loop-address   (:address *recur-binding-point*)

          [start, pset, suspend?]
          (partition-functional-expr op, expr, partition-address, address, params, make-call)

          same-partition (and (false? suspend?) (= partition loop-address))]
      (if same-partition
        [start, pset, false]  ; plain vanilla recur!
        (if suspend?
          [start, pset, true] ;; arguments suspended
          (if same-partition
            [expr, pset, false]
            [(make-call args), pset, false]))))))

;;
;;
;;
(defn partition-suspend-expr
  [expr, partition-addr, address, params]
  (let [op (first expr)
        [start, pset, _]
        (partition-functional-expr op expr partition-addr address params
          #(cons op %))]
    [start, pset, true]))

;;
;; Partitioning expressions with bindings
;;
(defn partition-functional-expr
  "Partitions expressions where an operator of some kind is applied to a sequence of
  arguments, evaluated left -> right. E.g., (op arg1 arg2 ...) or [arg1 arg2 arg3] etc.

  The caller supplies the usual arguments, and a function, make-call-form which takes a sequence
  of arguments to which the operator should be applied, and returns a valid s-expr."
  [op, expr, partition-addr, addr, params, make-call-form]
  {:pre [(vector? params)]}
  (letfn [(call-form [args]
            (with-meta (make-call-form args) (meta expr)))

          (call-form-with-bindings-and-args [keys args index]
            (apply call-form (concat (take-nth index keys))))]
    (let [[_ & args] expr
          keys    (make-implicit-parameters args)
          address (a/child addr op)

          [bind-start, bind-pset, bindings-suspend-count, final-pset, final-binding-index, final-params, final-addr]
          (partition-bindings-new keys, args, false, partition-addr, address, addr, params)]

      (cond
        ;; not suspending
        (zero? bindings-suspend-count) [(call-form args) nil false]

        ;; final pset can be substituted
        (= 1 (pset/size final-pset))
        (make-let-expr (bindings-at-index keys args final-binding-index)
          (if body-suspend? body-start body)))
      (if suspending?
        (let []
          [bind-start])
        [(call-form args) nil false]))))

#_(defn partition-functional-expr
    "Partitions a function-like expression - a list with an operator followed
    by an arbitrary list of arguments.

    Note: op is only used to generate an address
    make-call-form - is a function which takes a list of parameters and returns
    a form which represents the functional call"
    [op, expr, partition-address, address, params, make-call-form]
    {:pre [(vector? params)]}
    (letfn [(call-form [args]
              (with-meta (make-call-form args) (meta expr)))]
      (let [address    (a/child address op)
            [_ & args] expr
            value-expr (call-form args)
            keys       (make-implicit-parameters args)
            pcall-body [(call-form keys)]
            [start, pset, suspend?]
            (partition-bindings keys args partition-address address params
              pcall-body)]
        (if suspend?
          [start, pset, true] ; partition bindings has provided the correct result
          [value-expr, nil, false]))))

(defn make-let-expr
  "Returns a vector containing a let-binding expression [(let bindings"
  [bindings body]
  {:pre [(vector? body)]}
  (let [filtered-bindings (filter (fn [[k v]]
                                    (if (constant? k) ; accept [asdf :foo] but not [:foo asdf]
                                      (assert (= k v)) ; returns nil
                                      [k v]))
                            bindings)
        let-bindings      (vec (apply concat filtered-bindings))]
    (cond
      (-> let-bindings count (> 0)) `(let [~@let-bindings] ~@body)
      (-> body count (> 0)) `(do ~@body)
      :else (first body))))

(defn partition-bindings-new
  "Partitions a sequence of symbols and expressions representing bindings. This supports
  let-binding and function argument evaluation.

  Given keys= [s1..sn], exprs= [e1...en], accumulate?, p-addr, body-addr, addr, params,
  s1...sn = binding symbols
  e1...en = expressions
  accumulate? = boolean representing that binding k(i-1) should be available when evaluating e(i)
      this should be true for let binding and false for function arguments.
  p-addr = partition address of the currently accumulating partition
  body-addr = partition address containing the code body to be executed with the given bindings
    - for example, the body of a let expression, or an address which will contain the function invokation expression
      to be used with the given bindings
    - this can be the same as addr
  addr = address of the expression containing the bindings (used to generate unique addresses for each binding argument)
  params = vector of symbols in the current lexical environment

  Returns:
  [
    start - start expression
    pset - partition set
    suspending-count - number of suspending expressions encountered.
      an optimization can be applied to function expressions when a single suspending expression
      is provided as the first argument.
    final-partition-pset - partition set of the last suspending expression (may be nil)
    final-bindings-index - index into syms and args, bindings/evaluation which the caller is responsible for
                           This value is 0 when no suspending arguments are encountered or 1+ index of the final
                           suspending expression.
    final-params - params for the final partition (includes the input key of the last suspending expr)
    final-address - address of the final partition
  ]"
  [syms args accumulate? p-addr addr body-addr params]
  (if (empty? syms)
    [nil nil 0 nil [] [] params]
    (let [pset                (atom (pset/create))
          final-pset          (atom (pset/create)) ; this stores the final pset, which is reported separately from main pset
          partition-params    (atom params)
          current-bindings    (atom [])
          p-addr              (atom p-addr)
          arg-addr            (atom (a/child addr 0))
          start               (atom nil)
          scount              (atom 0) ; suspending-count
          final-binding-index (atom 0)
          pset-add-resume-at  (fn [pset p-addr next-addr start-expr bindings params input-key]
                                {:pre [(a/address? p-addr) (a/address? next-addr) (a/address? next-addr)
                                       (not (atom? start-expr)) (vector? bindings) (vector? params) (symbol? input-key)]}
                                (pset/add pset p-addr params
                                  [(make-let-expr bindings
                                     [`(resume-at [~next-addr ~params ~input-key]
                                         ~start-expr)])]
                                  true))]
      (loop [[sym & next-syms] syms
             [arg & next-args] args
             index 0]
        (let [[arg-start, arg-pset, suspend?] (partition-expr arg, @p-addr, @arg-addr, @partition-params)
              _          (swap! arg-addr a/increment)
              next-addr  (if (empty? next-args) body-addr @arg-addr)
              next-index (inc index)]
          (if suspend?
            ;; create a new partition
            (do
              ;; merge the previous final-pset into the main pset
              (swap! pset pset/combine @final-pset)

              ;; the arg-pset is the new final-pset
              (reset! final-pset arg-pset)

              (if start)
              (pset-add-resume-at arg-pset @p-addr next-addr arg-start @current-bindings @partition-params sym)

              (reset! final-binding-index next-index) ;; bindings in the current partition
              (if accumulate?
                (swap! partition-params (comp vec concat) (conj (mapv first @current-bindings) sym)))
              (reset! current-bindings [])
              (reset! p-addr next-addr)
              (swap! scount inc)
              (swap! start #(or % arg-start)))

            ;; if arg doesn't suspend, just accumulate the binding
            (swap! current-bindings conj [sym arg]))

          (swap! arg-addr a/increment)

          (if-not (empty? next-syms)
            (recur next-syms next-args next-index))))
      [@start, @pset, @scount, @final-pset, @final-binding-index, @partition-params, @p-addr])))

(defn bindings-at-index [syms args index]
  (interleave (nthrest syms index) (nthrest args index)))

(defn partition-bindings
  "Partitions the bindings, and executes `body` in that context. Note:
  this function does not partition `body` - it merely pastes the supplied
  forms into the appropriate location after the bindings.

  The caller should separately partition forms and supply the `start` result
  as the `body` argument.

  Note: the caller may pass constants for k, which indicates that the binding can be eliminated
        this function also eliminates re-binding (where k is the same as v).
  Args:
  Returns: [start, pset, suspend?]"
  [keys, args, partition-address, address, params, body]
  {:pre [(a/address? partition-address)
         (a/address? address)
         (vector? params)
         (vector? body)]}
  (with-tail-position false
    (let [dirty-address partition-address
          add-binding   (fn [bindings k v]
                          {:pre  [(vector? bindings)]
                           :post [(vector? bindings) (every? vector? bindings)]}
                          ;; eliminate constants and rebinding
                          (if (or (constant? k) (= k v)) bindings (conj bindings [k v])))]
      (loop
        [[key & rest-keys] keys
         [arg & rest-args] args
         current-bindings  [] ; the new bindings introduced to the partition
         partition-address partition-address
         arg-address       (a/child address 0) ; address of the arg
         params            params ; params provided to this partition
         start             nil
         any-suspend?      false
         pset              (pset/create)]
        (assert (not (and (nil? key) (not (empty? rest-keys)))))
        (if key
          (let [[arg-start, arg-pset, suspend?]
                (partition-expr arg, partition-address, arg-address, params) ;

                pset         (pset/combine pset arg-pset)
                next-address (a/increment arg-address)
                key          (partition-macroexpand key) ; substitute gensyms
                new-params   (if-not (constant? key)
                               (conj params key)
                               params)]
            (if suspend?
              (let [resume-pexpr `(resume-at [~next-address [~@params] ~key]
                                    ~arg-start)
                    pexpr        (first (make-let-body current-bindings [resume-pexpr]))
                    pset         (pset/add pset partition-address params [pexpr])
                    start        (or start pexpr)]
                (recur rest-keys, rest-args, [], next-address, next-address,
                  new-params, start, (or suspend? any-suspend?), pset))
              (recur rest-keys, rest-args
                (add-binding current-bindings key arg-start),
                partition-address, next-address,
                new-params, start, any-suspend?, pset)))

          ;; finalize the last partition by executing the body with the
          ;; bound params
          (let [final-body (make-let-body current-bindings body)
                clean-pset (pset/delete pset dirty-address)
                start      (or start (first final-body))
                pset       (pset/add clean-pset partition-address params final-body)]
            [start, pset, any-suspend?]))))))

;;
;; Many partitioners use the functional partitioner
;;
(defn partition-map-expr
  [expr partition-addr address params]
  {:pre [(map? expr)
         (a/address? partition-addr)
         (a/address? address)
         (vector? params)]}
  (let [fake-op     (symbol "#map")
        pseudo-expr (cons fake-op (apply concat (map identity expr)))]
    (partition-functional-expr fake-op pseudo-expr partition-addr address params
      (fn [args] (into {} (map vec (partition 2 args)))))))

(defn partition-vector-expr
  [expr partition-addr address params]
  (let [fake-op      (symbol "#vec")
        expr-with-op (with-meta `(~fake-op ~@expr) (meta expr))]
    (partition-functional-expr fake-op expr-with-op partition-addr address params
      #(vec %))))

(defn partition-set-expr
  [expr partition-addr address params]
  (let [fake-op      (symbol "#set")
        expr-with-op (with-meta `(~fake-op ~@expr) (meta expr))]
    (partition-functional-expr fake-op expr-with-op partition-addr address params
      #(set %))))

;; Clojure case* is optimized for constant time look up, but it's a pain to
;; partition. My strategy is to just convert to a cond expression. This can't
;; be done with case*, so we just have to throw an error in the unlikely case
;; that a user actually writes that by hand.
(defn partition-case-expr [expr, partition-addr, address, params]
  (let [[_ e & clauses] expr
        [clauses, default] (if (odd? (count clauses))
                             [(butlast clauses), (last clauses)]
                             [clauses, nil])
        evar         (gensym)
        cond-clauses (apply concat (map (fn [[left right]] [`(= ~evar ~left) right]) (partition 2 clauses)))
        let-expr     (with-meta `(let [~evar ~e]
                                   (cond ~@cond-clauses ~@(if default `(:otherwise ~default))))
                       (meta expr))]
    (partition-expr let-expr, partition-addr, address, params)))

(defn partition-case*-expr [expr & _]
  (throw-partition-error "Unable to partition case* expression. Please use case or cond instead" expr))

(defn partition-callcc-expr
  [exp, partition-addr, address, params]
  (let [[op & args] exp
        _     (assert (callcc-op? op))
        [f & _] args
        f     (or f `clojure.core/identity)
        faddr (a/child address 'callcc 0)
        [fstart, fpset, _] (partition-expr f, partition-addr, faddr, params)]
    (if (-> args count (> 1))
      (throw (ArityException. (count args) "rapids/callcc")))
    [`(let [stack#        (rapids.runtime.runlet/current-run :stack)
            dynamics#     (rapids.runtime.runlet/current-run :dynamics)
            fstart#       ~fstart
            continuation# (rapids/fcall rapids.runtime.cc/make-current-continuation stack# dynamics#)]
        (rapids/fcall fstart# continuation#)), fpset, true]))

(defn partition-flow-invokation-expr
  [op, expr, partition-addr, address, params]
  (let [[start, pset, _] (partition-functional-expr op expr partition-addr address params
                           (fn [args]
                             `(rapids/fcall ~op ~@args)))]
    [start, pset, true]))

(defn partition-fncall-expr
  [op, expr, partition-addr, address, params]
  {:pre [(vector? params)]}
  (partition-functional-expr op expr partition-addr address params
    (fn [args] `(~op ~@args))))

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
   (let [sigs             (extract-signatures fdecl)
         entry-point-name (str (a/to-string address) "__entry-point")
         [psets, sig-defs] (reverse-interleave
                             (apply concat
                               (map-indexed
                                 (fn [idx sig]
                                   ()
                                   (partition-signature m (a/child address idx) sig params))
                                 sigs))
                             2)
         pset             (apply pset/combine psets)
         entry-fn-def     `(fn ~(symbol entry-point-name) ~@sig-defs)]
     [entry-fn-def, pset])))

(defn partition-signature
  "Returns a pset and and a partitioned sig definition.

  For example, this function definition has 2 signatures aka
  E.g., (defn foo
          ([] (foo :bar))
          ([msg] (println \"foo\" msg))"
  [m address sig params]
  (let [[args & code] sig
        params         (vec (concat (params-from-args args [] (-> m :line)) params))
        _              (exclude-from-gensym-replacement params)
        [start-body, pset, _] (partition-body (vec code) address address params)
        pset           (pset/add pset address params start-body)
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
