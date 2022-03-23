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
;;         (:retry [] (measure-inr-level)) ;; equivalent to {:name :retry :do (flow [] get-cholesterol-level)})
;;         {:name :recompute
;;          :do (flow [v] ...)
;;          :describe #(...)
;;          :data {}})
;;
;;   ;; handlers - can run some code, and can either invoke a retrace or a recovery
;;                 note that during an interruption, the caller with the interruption ID
;;                 has control; the run is outputing to that caller.
;;
;;   (handle :abort i  ;; e.g., returning a different value
;;      (>* "Hello, doctor, I am aborting this dosing procedure")
;;      nil) ; return nil from this attempt block
;;
;;   (handle :warfarin-sensitivity-change i  ;; e.g., retracing to an earlier step
;;      (>* (str "Hello, doctor, I will reset the dose to " (:new-dosage i) " as you requested")
;;      (restart :set-dosage (:new-dosage i)) ; retraces are defined in the attempt
;;
;;   (handle :retry-dosing i  ;; recovers from the interrupted step
;;      (restart i :retry)) ; recoveries are defined in the interrupt
;;
;;   (handle true i ;; catch any Interruption - demonstrates handling an arbitrary process
;;      (>* "I'm unable to determine what to do next. Please select one of the choices.")
;;      (>* (generate-choices-from-restarts (concat )))
;;   (finally ...))

(ns rapids.language.attempt
  (:require [rapids.language.flow :refer [flow]]
            [rapids.objects.interruptions :refer :all]
            [rapids.runtime.calling :refer [universal-call]]
            [rapids.runtime.globals :refer :all]
            [rapids.runtime.runlet :refer [update-run!]]
            [rapids.support.util :refer :all]))

(defn handler-form? [o] (and (seq? o) (= 'handle (first o))))
(defn finally-form [o] (and (seq? o) (= 'finally (first o))))
(defn attempt-subclause? [o] (or (handler-form? o) (finally-form o)))

(defn expand-handler [ccvar h-form finally-flow]
  (let [[_ i-name ivar & body] h-form
        m    (meta h-form)
        line (if-let [lnum (:line m)] (str " at line " lnum) "")]
    (assert (keyword? i-name)
      (str "First argument to attempt handler should be a keyword" line))
    (assert (simple-symbol? ivar)
      (str "Second argument to attempt handler should be an unqualified symbol" line))
    `(->InterruptionHandler ~i-name
       (flow [~ivar]
         (rapids/fcall ~ccvar (let [result# (do ~@body)]
                                ~@(if finally-flow `((rapids/fcall ~finally-flow)))
                                (update-run! :interrupt nil)
                                result#))))))

(defn normalize-restart-def [r]
  (let [restart (cond
                  (seq? r) (let [[name & body] r
                                  [description & sigs] (if (-> body first string?)
                                                         body `(nil ~@body))]
                              {:name        name
                               :description description
                               :data        nil
                               :do          `(~'flow ~@sigs)})
                  (map? r) r)
        {name        :name,
         description :description,
         doflow      :do} restart]
    (assert (keyword? name) (str "Restart name must be a keyword: " name))
    (assert ((some-fn string? fn? nil?) description) (str "If provided, restart escription must be string or function" description))
    (assert (and (seq? doflow) (= 'flow (first doflow))) (str "Invalid do clause in restart: " doflow))
    restart))

(defn generate-restart-map [restartdefs restart-cc]
  (let [make-restart (fn [nrdef]
                       `(map->Restart
                          {:name         ~(:name nrdef)
                           :data         ~(:data nrdef)
                           :description  ~(:description nrdef)
                           :continuation (rapids/flow [& args#]
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
  (let [body         (doall (take-while #(not (attempt-subclause? %)) forms))
        handlers     (doall (take-while handler-form? (nthrest forms (count body))))
        final-forms  (doall (nthrest forms (+ (count body) (count handlers))))
        _            (assert (<= (count final-forms) 1) (str "Unexpected forms in attempt block" final-forms))
        finally-body (doall (rest (first final-forms)))
        attempt-cc   (gensym "attempt-cc")
        finally-flow (if-not (empty? finally-body) (gensym "finally"))]
    `(rapids/callcc
       (rapids/flow [~attempt-cc]
         (let [~@(if finally-flow
                   [finally-flow `(rapids/flow [] ~@finally-body)])]
           (binding [*attempts* (conj *attempts* (->Attempt
                                                   [~@(map #(expand-handler attempt-cc % finally-flow) handlers)]
                                                   {}))]
             ~@(if finally-flow
                `((let [result# (do ~@body)]
                    (rapids/fcall ~finally-flow)
                    result#))
                 body)))))))

(defmacro handle [iname ivar & body]
  (throw (ex-info "Attempt handler must appear at end of attempt body and before finally clause."
           {:form &form
            :line (:line (meta &form))})))

(defmacro restartable
  "Executes expr and provides one or more restarts. Each recoverable is a flow which takes
  a value and returns a value which will be provided at the location of the recoverable.

  E.g.,
  (let [val (recoverable (calculate-dosage p)
              (set-dosage [d] d)
              (recalculate-dosage-with-params [p] (calculate-dosage p)))
     ...)"
  [expr & restarts]
  ;(if (not (bound? *attempts*))
  ;  (throw (ex-info (str "Defining a restart is only allowed in an attempt body:" &form)
  ;           {:form &form})))
  (let [restart-cc  (gensym "restart-cc")
        restart-map (generate-restart-map restarts restart-cc)]
    `(rapids/callcc (rapids/flow [~restart-cc]
                      ~restart-map
                      ~expr))))


(defn attempt-bound? [] (not (empty? *attempts*)))

(defn restart [rname & args]
  {:pre [(attempt-bound?) (keyword? rname)]}
  (let [{restarts :restarts} (first *attempts*)]
    (if-not (contains? restarts rname)
      (throw (ex-info "Unknown restart"
               {:type    :runtime-error
                :restart rname})))
    (universal-call (get-in restarts [rname :continuation]) args)))