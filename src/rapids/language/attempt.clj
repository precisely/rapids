;;
;; This file describes the attempt/handle/finally feature. It's analogous to Clojure's try/catch/finally,
;; but operates over suspending bodies.
;;
;; It is composed of the `attempt` macro which provides `handle` and `finally` internal forms, and
;; a context in which restartable expressions may appear.
;;
;; Restarts can be invoked within handle clauses
;;
;;
;; (attempt
;;   (let [dosage (restartable (calculate-dosage)
;;                  ; shorthand form:
;;                  (:set-dosage "Describe it here" [] ...flow-body)
;;                  ; alternative longhand form:
;;                  {:name :set-dosage
;;                   :do (flow [..] ...), ; if not provided, defaults to (flow [] (the-expression))
;;                   :describe #(... return a string)
;;                   :data {}
;;                   :expose true})] ; if true, this codepoint will be appended to
;;                                  ; the interruption's restarts and thus will be available
;;                                  ; outside this attempt block
;;      (advise-patient-on-dosage dosage)
;;      (do-other-stuff)
;;
;;      ;; define multiple restarts within an attempt body
;;      (restartable (measure-inr-level..)
;;         (:retry [] (measure-inr-level)) ;; equivalent to {:name :retry :do (flow [] (measure-inr-level))})
;;         {:name :recompute
;;          :do (flow [v] ...)
;;          :describe #(...)
;;          :data {}})
;;
;;   ;; handlers - can run some code, and can either invoke a retrace or a recovery
;;                 note that during an interruption, the caller with the interruption ID
;;                 has control; the run is outputing to that caller.
;;
;;   (handle :abort [i]  ;; e.g., returning a different value
;;      (>* "Hello, doctor, I am aborting this dosing procedure")
;;      nil) ; return nil from this attempt block
;;
;;   (handle :warfarin-sensitivity-change  ;; retracing to an earlier step
;;      "Warfarin sensitivity has changed" ; an optional :doc string can be provided
;;      {:foo true, :bar false} ; an optional data map can be provided
;;      [i]
;;      (>* (str "Hello, doctor, I will reset the dose to " (:new-dosage i) " as you requested")
;;      (restart :set-dosage (:new-dosage i)) ; retraces are defined in the attempt
;;
;;   (handle :retry-dosing [i]  ;; recovers from the interrupted step
;;      (restart i :retry)) ; recoveries are defined in the interrupt
;;
;;   (handle true [i] ;; catch any Interruption - demonstrates handling an arbitrary process
;;      (>* "I'm unable to determine what to do next. Please select one of the choices.")
;;      (>* (generate-choices-from-restarts (concat )))
;;   (finally ...))

(ns rapids.language.attempt
  (:require [rapids.language.flow :refer [flow normalize-deflow-args]]
            [rapids.objects.interruptions :refer :all]
            [rapids.runtime.calling :refer [universal-call]]
            [rapids.runtime.globals :refer :all]
            [rapids.runtime.runlet :refer [update-run!]]
            [rapids.support.util :refer :all]
            [rapids.objects.run :as r]
            [rapids.runtime.runlet :as rt]))

(defn handler-form? [o] (and (seq? o) (= 'handle (first o))))
(defn finally-form [o] (and (seq? o) (= 'finally (first o))))
(defn attempt-subclause? [o] (or (handler-form? o) (finally-form o)))

(defn normalize-handler-args
  "Parses args as [docstring? attrmap? body] and returns

   [attrmap arglist body], where docstring is included in attrmap in the :doc key."
  [args]
  (let [[docstring attrmap sigs] (normalize-deflow-args args)
        _       (if (-> sigs count (> 1))
                  (throw (ex-info "Handler body must be a single arity flow"
                           {:type :syntax-error})))
        [arglist & body] (first sigs)
        _       (if-not (vector? arglist)
                  (throw (ex-info "Expecting a vector for handler arglist"
                           {:type    :syntax-error
                            :arglist arglist})))
        attrmap (if docstring (assoc attrmap :doc docstring) attrmap)]
    [attrmap arglist body]))

(defn expand-handler [ccvar h-form finally-flow]
  {:pre [(seq? h-form)]}
  (let [[_ iname & definition] h-form
        m        (meta h-form)
        err-data (merge m {:type :syntax-error})
        _        (if-not (keyword? iname)
                   (throw (ex-info "Invalid interruption handler name: expecting a keyword"
                            (assoc err-data :name iname :type (type iname)))))

        [metadata arglist body] (normalize-handler-args definition)
        metadata (assoc metadata :_source (assoc m :file *file*))]
    (if (-> arglist count (> 1))
      (throw (ex-info "Only one argument to interruption handler allowed"
               (assoc err-data :arglist arglist))))
    `(->InterruptionHandler ~iname
       (flow ~arglist
         (rapids/fcall ~ccvar (let [result# (do ~@body)]
                                ~@(if finally-flow `((rapids/fcall ~finally-flow)))
                                (update-run! :interrupt nil)
                                result#)))
       ~metadata)))

(defn normalize-restart-def [r]
  (let [restart (cond
                  (seq? r) (let [[name & definition] r
                                 [metadata arglist body] (normalize-handler-args definition)
                                 m        (meta r)
                                 metadata (assoc metadata :_source (assoc m :file *file*))]
                             {:name     name
                              :metadata metadata
                              :do       `(~'flow ~arglist ~@body)})
                  (map? r) r)
        {name       :name,
         {doc :doc} :metadata,
         doflow     :do} restart]
    (if-not (keyword? name)
      (throw (ex-info "Restart name must be a keyword" {:type :syntax-error, :restart r})))
    (if-not ((some-fn string? nil?) doc)
      (throw (ex-info "If provided, restart doc must be string"
               {:type :syntax-error, :restart r})))
    (if-not (and (seq? doflow) (= 'flow (first doflow)))
      (throw (ex-info "Invalid do clause in restart"
               {:type :syntax-error, :restart r})))
    restart))

(defn install-restart-map-expr [restartdefs restart-cc]
  (let [make-restart (fn [nrdef]
                       `(map->Restart
                          {:name     ~(:name nrdef)
                           :metadata ~(:metadata nrdef)
                           :closure  (rapids/flow [& args#]
                                       (rapids/fcall ~restart-cc
                                         (rapids/fapply ~(:do nrdef) args#)))}))
        nrestartdefs (map normalize-restart-def restartdefs)
        restart-map  `(hash-map ~@(apply concat (map #(vector (:name %), (make-restart %)) nrestartdefs)))]
    `(set! *attempts* (cons (-> *attempts* first (update :restarts merge ~restart-map))
                        (rest *attempts*)))))

;;
;; attempt unrolls to a callcc form which establishes interruption handlers
;;
(defmacro attempt [& forms]
  (let [body              (doall (take-while #(not (attempt-subclause? %)) forms))
        handlers          (doall (take-while handler-form? (nthrest forms (count body))))
        final-forms       (doall (nthrest forms (+ (count body) (count handlers))))
        _                 (assert (<= (count final-forms) 1) (str "Unexpected forms in attempt block" final-forms))
        finally-body      (doall (rest (first final-forms)))
        attempt-cc        (gensym "attempt-cc")
        finally-flow      (if-not (empty? finally-body) (gensym "finally"))
        expanded-handlers (mapv #(expand-handler attempt-cc % finally-flow) handlers)]
    `(rapids/callcc
       (rapids/flow [~attempt-cc]
         (let [~@(if finally-flow
                   [finally-flow `(rapids/flow [] ~@finally-body)])]
           (binding [*attempts* (conj *attempts* (->Attempt
                                                   ~expanded-handlers
                                                   {}))]
             ~@(if finally-flow
                 `((let [result# (do ~@body)]
                     (rapids/fcall ~finally-flow)
                     result#))
                 body)))))))

(defmacro ^{:arglists '([name doc-string? attr-map? [data] body])}
  handle
  "Installs an interrupt handler for the current attempt.

  Usage:
  (handle name doc-string? attr-map? [data] body)
  name        - keyword naming the interruption
  doc-string? - optional string
  metadata?   - optional map
  data        - A symbol or destructuring expression which will bind the interrupt's :data
                and will be accessible to body
  Example:
  (handle :foo
    \"this is the docstring - it will be :doc in the :metadata\"
    {:my-meta-data \"this map will be accessible as :metadata\"}
    [interrupt-data]
    (do-something-with interrupt-data))

  When (interrupt! run :foo 123) is called, the :foo handler will be invoked and interrupt-data will be 123."
  [& args]
  (throw (ex-info "Attempt handler must appear at end of attempt body and before finally clause."
           {:form &form
            :line (:line (meta &form))})))

(defmacro restartable
  "Stores a point in code and makes it available to subsequent restart calls.

  Takes an expr, which it executes and returns the value of, and provides one or more restarts.
  Each restart is a flow which takes any number of args and returns a value which will be provided at the location of the
  restartable expression. Restarts are invoked by calling `(restart restart-name ...args)`.

  Unlike handlers, which handle situations arising within the attempt block they are defined in,
  restarts, once enountered, are available until the outermost attempt block completes.

  `(list-restarts)`

  Usage:
  (restartable expr
    (name doc-string? metadata? [args] body)
    (name doc-string? metadata? [args] body)
    ...)

  name        - keyword
  doc-string? - optional string
  metadata?   - optional map
  [args]      - arglist
  body        - flow body

  Example:
  (attempt
    (restartable (calculate-dosage p)
      (:set-dosage
        \"Sets the dosage\" {:interactive false}
        [dosage] dosage)
      (:redo-dosage
        \"Reinterviews the patient to determine dosage\" {:interactive true}
        [patient message]
        (notify-patient patient message)
        (calculate-dosage patient)))

    (monitor-patient...)

    (handle :adverse-event [i]
      (restart :set-dosage 10))

    ...
    (handle ...
      (restart ...))"
  [expr & restarts]
  ;(if (not (bound? *attempts*))
  ;  (throw (ex-info (str "Defining a restart is only allowed in an attempt body:" &form)
  ;           {:form &form})))
  (let [restart-cc          (gensym "restart-cc")
        install-restart-map (install-restart-map-expr restarts restart-cc)]
    `(rapids/callcc (rapids/flow [~restart-cc]
                      ~install-restart-map
                      ~expr))))

(defn list-interrupt-handlers
  "Provides a sequence of interrupt handler objects in order of precedence. Handlers from innermost
  attempt blocks are returned first."
  ([] (list-interrupt-handlers rapids.runtime.globals/*attempts*))
  ([obj]
   (let [attempts (if (rt/run? obj)
                    (r/get-dynamic-value obj 'rapids.runtime.globals/*attempts*)
                    rapids.runtime.globals/*attempts*)]
     (reverse (apply concat (map :handlers attempts))))))

(defn list-restarts
  "Returns a sequence of restarts available for the current attempt.

  Usage:
  (list-restarts) ; restarts available in the current dynamic environment
  (list-restarts run) ; restarts available in the current dynamic environment of the run"
  ([] (list-restarts rapids.runtime.globals/*attempts*))
  ([obj]
   (let [attempts (if (rt/run? obj)
                    (r/get-dynamic-value obj 'rapids.runtime.globals/*attempts*)
                    obj)]
     (-> attempts last :restarts vals))))

(defn attempt-bound? [] (not (empty? *attempts*)))

(defn restart [rname & args]
  {:pre [(attempt-bound?) (keyword? rname)]}
  (let [{restarts :restarts} (first *attempts*)]
    (if-not (contains? restarts rname)
      (throw (ex-info "Unknown restart"
               {:type    :runtime-error
                :restart rname})))
    (universal-call (get-in restarts [rname :closure]) args)))