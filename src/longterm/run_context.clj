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
            [longterm.signals :as s]))
(def ^{:dynamic true
       :doc     "The id of the run that initiated the current runlet (which may be a child or parent)"}
  *root-run-id*)

(def ^{:dynamic true
       :doc     "The id of the current run (which may differ from *root-run* because of redirects or returns from redirects)"}
  *current-run-id*)

(def ^{:dynamic true
       :doc     "A map of run-ids to runs"}
  *run-cache*)

(declare load! save-cache! cache-run! update-run! finalize-run!
  set-error! simulate-result)

(defmacro with-run-cache [& body]
  "Sets up the dynamic environment for the runlet (which may include several runs);
  Runs are persisted to disk at the last moment, when the outer-most with-run-cache
  body completes."
  `(letfn [(dobody# [] (assert (bound? #'*run-cache*)) ~@body)]
     (if (bound? #'*run-cache*)
       (dobody#)
       (binding [*run-cache* {}]
         (let [result# (dobody#)]
           (save-cache!)
           result#)))))

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
         (finalize-run! *root-run-id* *current-run-id*)))))

;;;
;;; Cache operations
;;;
(defn save-cache! []
  (doseq [[_ run] *run-cache*]
    (when (:dirty run)
      (cache-run! (dissoc run :dirty))
      (assert (rs/run-in-state? run :error :complete :suspended))
      (rs/save-run! run))))

(defn cache-run! [run]
  (assert (bound? #'*run-cache*))
  (set! *run-cache* (assoc *run-cache* (:id run) run))
  run)

(defn check-permit [run permit]
  (if (not= (-> run :suspend :permit) permit)
    (throw (Exception. "Invalid permit provided for run " (:id run)))))

(defn acquire!
  "Atomically acquires the run in :running state - from the cache or storage"
  [run-id permit]
  (letfn [(acquire-from-cache! []
            (ifit [retrieved (get *run-cache* run-id)]
              (case (:state retrieved)
                :suspended (do (check-permit retrieved permit)
                               (cache-run! (assoc retrieved :state :running, :suspend nil)))
                :running retrieved
                (throw (Exception. (str "Cannot acquire run " run-id " from cache in state " (:state retrieved)))))))]
    (let [cached-run (acquire-from-cache!)]
      (or cached-run
        ;; when acquiring from the runstore, reset the response:
        (cache-run! (assoc (rs/acquire-run! run-id permit) :run-response []))))))

(defn load!
  "Gets the run from the cache or storage"
  [run-id]
  (or
    (get *run-cache* run-id)
    (ifit [run (rs/get-run run-id)]
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
  (rs/run-in-state? (current-run) :suspended))

(defn return-mode?
  [& modes]
  (apply rs/run-in-mode? (current-run) modes))

(defn redirected?
  ([] (redirected? (current-run)))

  ([run]
   (and (rs/run-in-state? run :suspended)
     (ifit (:suspend run)
       (= (:mode it) :redirect)))))

(defn blocked?
  ([] (blocked? (current-run)))
  ([run]
   (and (rs/run-in-state? run :suspended)
     (ifit (:suspend run)
       (= (:mode it) :block)))))

;;
;; modifiers
;;
(defn initialize-runlet [initial-response]
  (update-run! #(assoc % :suspend nil, :run-response initial-response)))

(defn push-stack! [address bindings result-key]
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
         (assoc %
           :state :suspended,
           :suspend suspend)))
    (if (redirected?)
      (s/make-return-signal)  ; returns a Control signal
      suspend)))              ; returns a Suspend signal

(defn set-error! [e]
  (update-run! #(assoc %, :state :error, :error e)))

;;
;; redirection and return - these functions transfer response control back and forth
;;
(declare redirect-to-suspended-run! transfer-control-post-facto! redirect-to-completed-run! add-response-from!)
(defn set-redirect! [child-run expires default]
  "Transfers responsibility to child-run from current run. Note that this function transfers control
  to the child-run *after* it has completed a runlet. So this run will be decorated with a next-id, indicating
  a potential transfer of control. Thus, the redirection may not be directly to child-run, but to another
  run which child-run spawned."
  {:pre [(not (rs/run-in-state? child-run :running :error))]}
  (case (:state child-run)
    :suspended (if (blocked? child-run)
                 child-run
                 (redirect-to-suspended-run! child-run expires default))
    :complete child-run))     ;; do not actually redirect - just return the run

(defn return-from-redirect!
  "Transfers control back to the parent run (which has already been run to suspension or completion) and
  returns the appropriate runloop signal or a value."
  [run]
  {:pre [(= (:id run) (parent-run-id))
         (= :redirect (return-mode))]}
  (update-run! #(assoc % :parent-run-id nil return-mode nil))
  (transfer-control-post-facto! run))


;;
;; Helpers
;;
(defn- redirect-to-suspended-run!
  "Suspends the current run,"
  [child-run expires default]
  {:pre [(rs/run-in-state? child-run :suspended)
         (:next-id child-run)
         (-> child-run :parent-run-id nil?)]}
  (let [child-run (update-run! child-run #(assoc %
                                            :parent-run-id (id),
                                            :return-mode :redirect))]

    (update-run!
      #(do (assert (rs/run-in-state? % :running))
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
  {:pre [(rs/run-in-state? run :complete :suspended)]}
  (let [next-run-id (-> run :next-id)]
    (add-response-from! run)
    (set! *current-run-id* next-run-id)
    (simulate-result (load! next-run-id))))

(defn- add-response-from!
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
         new-run (if-not (= run new-run) (assoc new-run :dirty true) new-run)]
     (cache-run! new-run))))

(defn finalize-run!
  ""
  [run-id next-run-id]
  (let [run      (get *run-cache* run-id)
        next-run (get *run-cache* next-run-id)]
    (cache-run!
      (assoc run
        :next-id (or (:id next-run) (:id run))))))

