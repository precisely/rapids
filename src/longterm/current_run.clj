;;
;; Functions for managing the hierarchical data structure which represents the current run.
;; The *run* dynamic var holds the root run (bound by start! or continue!).
;; The :next key is set to another run to signify a transfer of control, either redirection
;; or a return from a redirection. Redirection is conceptually analogous to the Unix concept
;; of fork-exec.
;;
(ns longterm.current-run
  (:require [longterm.util :refer :all]
            [longterm.runstore :as rs]))

(def ^:dynamic *run*)

(defn current-run
  "Returns the inner most :next run (always a run in :redirect mode which is in :suspended state)
  or the root run"
  []
  {:post [(or (nil? %) (rs/run-in-state? % :suspend))
          (or (rs/run-in-mode? % :redirect)
            (and (rs/run-in-state? % :default)
              (= *run*)))]}
  (letfn
    [(next-run [run]
       (ifit (:next run) (recur it) run))]
    (if (bound? #'*run*) (next-run *run*))))

(defn alter-current-run!
  "Alters the current run, which may be a nested :next run, but ensuring that *run* is updated"
  [f]
  (letfn [(alter-it [run f]
            (ifit (:next run)
              (assoc run :next (alter-it it f))
              (f run)))]

    (if (bound? #'*run*)
      (set! *run* (alter-it *run* f))
      (throw (Exception. "Attempt to alter current run, but no run is defined")))))

(defn push-stack [frame]
  (alter-current-run! #(assoc %
                         :dirty true
                         :stack (cons frame (:stack %)))))

(defn pop-stack []
  (let [return (atom nil)]
    (alter-current-run! #(let [[frame & rest-stack] (:stack %)]
                           (reset! return frame)
                           (assoc %
                             :dirty (or frame (not (empty? rest-stack)))
                             :stack rest-stack)))
    @return))

(defn stack [] (:stack (current-run)))

(defn mode [] (:mode (current-run)))

(defn response [] (:response (current-run)))

(defn result [] (:result (current-run)))

(defn set-block! [child expires result]
  (alter-current-run!
    #(assoc %
       :dirty true,
       :mode :block,
       :suspend (->Suspend (:id child) expires result))))

(defn add-response! [r]
  (alter-current-run!
    #(let [current-response (:response %)]
       (assert (vector? current-response))
       (assoc % :dirty true :response (conj current-response r)))))

(defn set-result! [result]
  (alter-current-run! #(assoc % :dirty true :result result)))

(defn set-redirect! [child expires result]
  (alter-current-run!
    #(let [child (assoc child :parent-run-id (:id %) :mode :redirect)]
       (assert (rs/run-in-state? % :running))
       (assert (rs/run-in-mode? :default))
       (assoc %
         :dirty true,
         :next :state,
         :suspended child,
         :suspend (->Suspend (:id child) expires result)))))

(defn add-child! [child]
  (alter-current-run!
    #(let [current-children (or (:children %) [])]
       (assoc % :children (conj current-children child)))))

(defn suspend! [permit expires result]
  (alter-current-run!
    #(assoc %
       :dirty true,
       :state :suspended,
       :suspend (->Suspend permit expires result))))
