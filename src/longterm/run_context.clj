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

(declare load! save-cache! cache-run! update-current! finalize-run!
  transfer-full-response! set-error! simulate-result)

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
     (let [run# (cache-run! ~run-form)
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

(defn acquire!
  "Atomically acquires the run in :running state - from the cache or storage"
  [run-id permit]
  (let [retrieved (get *run-cache* run-id)]
    (if (rs/run-in-state? retrieved :running)
      retrieved
      ;; when acquiring from the runstore, reset the response:
      (cache-run! (assoc (rs/acquire-run! run-id permit) :response [])))))

(defn load!
  "Gets the run from the cache or storage"
  [run-id]
  (or
    (get *run-cache* run-id)
    (ifit (rs/get-run run-id)
      (cache-run! it))))

;;;
;;; Current Run
;;;
(defn current-run
  "Returns the currently active run"
  []
  (load! *current-run-id*))

;;
;; accessors
;;
(defn id [] (:id (current-run)))

(defn stack [] (:stack (current-run)))

(defn return-mode [] (:return-mode (current-run)))

(defn response [] (:response (current-run)))

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
  (update-current! #(assoc % :suspend nil, :response initial-response)))

(defn push-stack! [address bindings result-key]
  (let [frame (sf/make-stack-frame address bindings result-key)]
    (update-current! #(assoc % :stack (cons frame (:stack %))))))

(defn pop-stack! []
  (let [popped-frame (atom nil)]
    (update-current! #(let [[frame & rest-stack] (:stack %)]
                        (reset! popped-frame frame)
                        (assoc % :stack rest-stack)))
    @popped-frame))

(defn add-responses! [& responses]
  (update-current!
    #(let [current-response (:response %)]
       (assert (vector? current-response))
       (assoc % :response (vec (concat current-response responses))))))

(defn set-result! [result]
  (update-current! #(assoc % :state :complete, :result result)))

(defn set-suspend! [suspend]
  (update-current! #(assoc % :state :suspended, :suspend suspend))
  suspend)

(defn set-listen! [permit expires default]
  "Sets the current run in suspend! state with the given permit, expiry and default value"
  (set-suspend! (s/make-suspend-signal permit expires default)))

;;
;; state change
;;

(defn return-redirect! []
  (let [current (current-run)
        parent  (acquire-parent!)]
    (set! *current-run-id* (:id parent))
    (transfer-full-response! current parent)
    (s/make-continue-signal parent current)))

(defn set-blocker! [child expires default]
  "Suspends the current run with child, returning the suspend signal"
  (let [suspend (make-suspend-signal (:id child) expires default)]
    (update-current!
      #(do
         (cache-run! (assoc child,
                       :return-mode :block,
                       :parent-run-id (:id %)))
         (assoc %
           :state :suspended,
           :suspend suspend)))
    (if (redirected?)
      (return-redirect!)      ; returns a Control signal
      suspend)))              ; returns a Suspend signal

(defn set-error! [e]
  (update-current! #(assoc %, :state :error, :error e)))

(defn set-redirect! [child-run expires default]
  "Transfers responsibility to child-run from current run. Note that this function transfers control
  to the child-run *after* it has completed a runlet. So this run will be decorated with a next-id, indicating
  a potential transfer of control. Thus, the redirection may not be directly to child-run, but to another
  run which child-run spawned."
  {:pre [(rs/run-in-state? child-run :suspended)]}
  (update-current!
    #(let [child-run (assoc child-run
                       :parent-run-id (:id %),
                       :return-mode :redirect)
           _         (assert (:next-id child-run))
           next-run  (load! (:next-id child-run))]
       (assert (rs/run-in-state? % :running))
       (assert (rs/run-in-mode? % nil))
       (transfer-full-response! % next-run)
       ; (cache-run! child) ; not necessary because transfer-full-response! does the caching
       (set! *current-run-id* (:id next-run))
       (assoc %
         :state :suspended,
         :suspend (make-suspend-signal (:id child-run) expires default))))

  ;; send the result back to the eval-loop to finalize
  (simulate-result (current-run)))

;;;
;;; HELPERS
;;;
(defn simulate-result
  "Returns the value returned by the last continuation of the given run"
  [run]
  (case (:state run)
    :suspended (:suspend run)
    :error (throw (:error run))
    :complete (:result run)
    (throw (Exception. (str "Run " (:id run) " in unexpected state: " (:state run))))))

(defn- update-current!
  "Alters the current run, which may be a nested :next run, but ensuring that *run* is updated"
  [f]
  (let [current (current-run)
        new-run (f current)
        new-run (if-not (= current new-run) (assoc new-run :dirty true) new-run)]
    (cache-run! new-run)))

(defn finalize-run!
  ""
  [run-id next-run-id]
  (let [run      (get *run-cache* run-id)
        next-run (get *run-cache* next-run-id)]
    (cache-run!
      (assoc run
        :full-response (:full-response next-run-id)
        :next-id (or (:id next-run) (:id run))))))

(defn- transfer-full-response! [from to]
  "Ensures that the full-response in the `from` run is transfered to the `to` run;
  the `from` run response will precede the `to` run response. The `from` full-response
  is cleared."
  {:pre [(rs/run-in-state? from :any) (rs/run-in-state? to :any)]}
  (let [full-response (concat (or (:full-response from) [])
                        (or (:full-response to) []))]
    (cache-run! (assoc from :full-response []))
    (cache-run! (assoc to :full-response full-response))
    full-response))
