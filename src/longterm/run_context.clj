;;
;; The run context is a set of dynamic bindings representing:
;; 1. the root-run - the run at the beginning of a request (e.g., as determined by start! or continue!)
;; 2. the current run, which may differ from the root run as a result of redirection or return from redirection
;; 3. any other runs which may have been created or modified during the request
;;
;; The run context maintains a cache of runs which are locked (i.e., set in :running state) at the
;; beginning of a request.
(ns longterm.run-context
  (:require [longterm.util :refer :all]
            [longterm.runstore :as rs]
            [longterm.stack-frame :as sf]
            [longterm.signals :refer [make-suspend-signal]]
            [longterm.signals :as s]
            [longterm.run :as r]
            [taoensso.nippy :refer [extend-freeze extend-thaw]])
  (:import [longterm.run Run]
           [java.util UUID]))

(def ^{:dynamic true
       :doc     "The id of the run that initiated the current runlet (which may be a child or parent)"}
  *root-run-id*)

(def ^{:dynamic true
       :doc     "The id of the current run (which may differ from *root-run* because of redirects or returns from redirects)"}
  *current-run-id*)

(def ^{:dynamic true
       :doc     "A map of run-ids to runs"}
  *run-cache*)

(declare load! save-cache! cache-run! update-run! set-error! simulate-result root-run current-run set-next!)

(defmacro with-run-cache [& body]
  "Sets up the dynamic environment for the runlet (which may include several runs);
  Runs are persisted to disk at the last moment, when the outer-most with-run-cache
  body completes."
  `(letfn [(dobody# [] (assert (bound? #'*run-cache*)) ~@body)]
     ;; TODO: (try
     (if (bound? #'*run-cache*)
       (dobody#)
       (binding [*run-cache* {}]
         (rs/tx-begin!)
         (let [result# (dobody#)]
           (save-cache!)
           (rs/tx-commit!)
           result#)))))
;; TODO: figure out exception handling strategy
;;(catch Exception e (rollback))))

(defmacro with-run-context
  "Executes body returning the Run produced by run-form, "
  [[run-form] & body]
  `(with-run-cache
     (let [run#    (cache-run! ~run-form)
           run-id# (:id run#)]
       (binding [*current-run-id* run-id#,
                 *root-run-id*    run-id#]
         ;(try
         ~@body
         ;(catch Exception e#
         ;  (set-error! e#)))
         (set-next! *root-run-id* *current-run-id*)))))

;;;
;;; Cache operations
;;;
(defn save-cache! []
  (doseq [[_ run] *run-cache*]
    (when (:dirty run)
      (cache-run! (dissoc run :dirty))
      (assert (r/run-in-state? run :error :complete :suspended))
      (rs/save-run! run))))

(defn cache-run! [run]
  (assert (bound? #'*run-cache*))
  (set! *run-cache* (assoc *run-cache* (:id run) run))
  run)

(defn acquire!
  "Acquires the run, setting it in :running state, and clearing the suspend object"
  [run-id permit]
  (let [retrieved (load! run-id)]
    (if-not (r/run-in-state? retrieved :suspended :created)
      (throw (Exception. (str "Cannot acquire Run " run-id " from cache in state " (:state retrieved)))))
    (if (not= (-> retrieved :suspend :permit) permit)
      (throw (Exception. (str "Cannot acquire Run " run-id " invalid permit"))))

    (cache-run! (assoc retrieved :state :running, :run-response [], :suspend nil))))

(defn load!
  "Gets the run from the cache or storage"
  [run-id]
  (or
    (get *run-cache* run-id)
    (ifit [run (rs/lock-run! run-id)]
      (cache-run! run))))

;;;
;;; Current Run
;;;
(defn current-run
  "Returns the currently active run"
  []
  (load! *current-run-id*))

(defn root-run
  "Returns the root run"
  []
  (load! *root-run-id*))

;;
;; accessors
;;
(defn id [] (:id (current-run)))

(defn stack [] (:stack (current-run)))

(defn return-mode [] (:return-mode (current-run)))

(defn response [] (:run-response (current-run)))

(defn result [] (:result (current-run)))

(defn current-suspend-signal [] (:suspend (current-run)))

(defn parent-run-id [] (:parent-run-id (current-run)))

(defn acquire-parent! []
  "If current run has a parent, return the parent run, otherwise nil"
  (ifit (parent-run-id) (acquire! it (id))))
;;;
;;; Predicates
;;;
(defn suspended? []
  (r/run-in-state? (current-run) :suspended))

(defn return-mode?
  ([& modes]
   (apply r/run-in-mode? (current-run) modes)))

(defn redirected?
  ([] (redirected? (current-run)))

  ([run]
   (and (r/run-in-state? run :suspended)
     (r/run-in-mode? run :redirect))))

(defn blocked?
  ([] (blocked? (current-run)))
  ([run]
   (and (r/run-in-state? run :suspended)
     (r/run-in-mode? run :block))))

;;
;; modifiers
;;
(defn initialize-runlet [initial-response]
  (update-run! #(assoc % :suspend nil, :response initial-response, :run-response initial-response)))

(defn push-stack! [address bindings result-key]
  {:post [(r/run? %)
          (linked-list? (:stack %))]}
  (let [frame (sf/make-stack-frame address bindings result-key)]
    (update-run! #(assoc % :stack (cons frame (:stack %))))))

(defn pop-stack! []
  (let [popped-frame (atom nil)]
    (update-run! #(let [[frame & rest-stack] (:stack %)]
                    (reset! popped-frame frame)
                    (assoc % :stack rest-stack)))
    @popped-frame))

(defn add-responses! [& responses]
  (letfn [(push-responses [field]
            #(let [current-response (field %)]
               (assert (vector? current-response))
               (assoc % field (into [] (concat current-response responses)))))]
    (update-run! (root-run) (push-responses :response))
    (update-run! (push-responses :run-response))
    responses))

(defn set-result! [result]
  (update-run! #(assoc % :state :complete, :result result)))

(defn set-suspend! [suspend]
  (update-run! #(assoc % :state :suspended, :suspend suspend))
  suspend)

(defn set-listen! [permit expires default]
  "Sets the current run in suspend! state with the given permit, expiry and default value"
  (set-suspend! (s/make-suspend-signal permit expires default)))

;;
;; state change
;;
(defn set-blocker! [child expires default]
  "Blocks the current run with child, returning the suspend signal or a block"
  (let [suspend (make-suspend-signal (:id child) expires default)]
    (update-run!
      #(do
         (cache-run! (assoc child,
                       :return-mode :block,
                       :parent-run-id (:id %)))
         (assoc % :state :suspended, :suspend suspend)))
    (if (redirected?)
      (s/make-return-signal)  ; returns a Control signal
      suspend)))              ; returns a Suspend signal

(defn set-error! [e]
  (update-run! #(assoc %, :state :error, :error e)))

;;
;; redirection and return - these functions transfer response control back and forth
;;
(declare redirect-to-suspended-run! transfer-control-post-facto! redirect-to-completed-run! harvest-response-from!)
(defn set-redirect! [child-run expires default]
  "Transfers responsibility to child-run from current run. Note that this function transfers control
  to the child-run *after* it has completed a runlet. So this run will be decorated with a next-id, indicating
  a potential transfer of control. Thus, the redirection may not be directly to child-run, but to another
  run which child-run spawned."
  {:pre [(not (r/run-in-state? child-run :error))]}
  (case (:state child-run)
    :suspended (if (blocked? child-run)
                 child-run
                 (redirect-to-suspended-run! child-run expires default))
    :complete child-run))     ;; do not actually redirect - just return the run

(defn return-from-redirect!
  "Transfers control back to the parent run (which has already been run to suspension or completion) and
  returns the appropriate runloop signal or a value."
  [parent-run]
  {:pre [(= (:id parent-run) (parent-run-id))
         (return-mode? :redirect)]}
  (let [child-id (id),        ; current run is the child - remember the id because transfer-control will change to parent
        result   (transfer-control-post-facto! parent-run)]
    ;; we must get the child run from the cache again because transfer-control has altered it
    ;; return it to default mode - not parent
    (update-run! (load! child-id) #(assoc % :parent-run-id nil, :return-mode nil))
    result))


;;
;; Helpers
;;
(defn- redirect-to-suspended-run!
  "Suspends the current run,"
  [child-run expires default]
  {:pre [(r/run-in-state? child-run :suspended)
         (:next-id child-run)
         (-> child-run :parent-run-id nil?)]}
  (let [child-run (update-run! child-run #(assoc %
                                            :parent-run-id (id),
                                            :return-mode :redirect))]

    (update-run!
      #(do (assert (r/run-in-state? % :running))
           (assoc %
             :state :suspended,
             :suspend (make-suspend-signal (:id child-run), expires, default))))

    (transfer-control-post-facto! child-run)))

(defn- transfer-control-post-facto!
  "Implements a post-facto change of control. The input run is assumed to have already reached completion or suspension.
  This function copies values over to the current run context to produce the same effect as if control
  had been transfered before the input run had reached completion or suspension.

  In effect, this means copying the :response from run, and using the run's :next-id as the new
  current-run.

  Returns the appropriate signal (or completion value) from the new run which has control."
  [run]
  {:pre [(r/run-in-state? run :complete :suspended)]}
  (let [next-run-id (-> run :next-id)]
    (harvest-response-from! run)
    (set! *current-run-id* next-run-id)
    (simulate-result (load! next-run-id))))

(defn- harvest-response-from!
  [run]
  (update-run! (root-run) #(assoc % :response (into [] (concat (:response %) (:response run)))))
  (update-run! run #(assoc % :response [])))

(defn- simulate-result
  "Returns the value returned by the last continuation of the given run"
  [run]
  (case (:state run)
    :suspended (:suspend run)
    :error (throw (:error run))
    :complete (:result run)
    (throw (Exception. (str "Run " (:id run) " in unexpected state: " (:state run))))))


(defn- update-run!
  "Alters the provided or current run, which may be a nested :next run, but ensuring that *run* is updated"
  ([f] (update-run! (current-run) f))

  ([run f]
   (let [new-run (f run)
         new-run (if (not= run new-run) (assoc new-run :dirty true) new-run)]
     (cache-run! new-run))))

(defn- remove-next!
  "Removes the next value of run and nulls out the next-id"
  [run] (update-run! run #(dissoc (assoc % :next-id nil) :next)))

(defn set-next!
  "Sets the next-id and next-run; next-id is set to the id of the next-run, if it exists, or
  to the current run's id, if it is in :suspended state (indicating that it can still be called)."
  [run-id next-run-id]
  (let [run      (get *run-cache* run-id)
        next-run (remove-next! (get *run-cache* next-run-id))]
    (update-run! run
      #(let [next-id (or (:id next-run)
                       (if (r/run-in-state? run :suspended)
                         (:id %)))]
         (assoc %
           :next-id next-id,
           :next (if (not= next-id run-id) next-run))))))

;;
;; nippy
;;
(extend-freeze Run ::run
  [run data-output]
  (let [run-id (prn-str (:id run))]
    (.writeUTF data-output run-id)))

(extend-thaw ::run
  [data-input]
  (let [thawed-str (.readUTF data-input)
        run-id (read-string thawed-str)]
    (if (bound? #'*run-cache*)
      (load! run-id)

      ;; special handling for when a run is retrieved outside of a run-cache context
      ;; just get the run without locking it or storing it in the cache
      (rs/get-run run-id))))
