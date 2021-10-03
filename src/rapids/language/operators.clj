(ns rapids.language.operators
  (:require [rapids.runtime.core :as rt]
            [rapids.objects.signals :as signals]
            [rapids.objects.startable :as callable]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [rapids.objects.startable :as startable])
  (:import (java.time LocalDateTime)
           (clojure.lang IFn)))

;;
;; Callable methods
;;
;; TODO: remove need to fcall dynamically bound flows and switch from Startable to universally using IFn
;;       it would be nice to get rid of this cruft and just implement the IFn
;;       protocol on Flows which would auto-detect if a run is present.
;;
;; The issue is that the partitioner would need to be able to figure out if
;; a variable holds a suspending operator. One option would be to assume
;; *every* operator is suspending unless we can prove otherwise. Some ideas
;; for rules:
;;
;; 1. If the op is in params, assume it is suspending and fcall it, otherwise...
;; 2. If the op is a flow, it is suspending. Wrap it in resume-at
;; 3. If the op is a function, it is not suspending. Just call normally.
;; 4. If the op is a set, map or keyword, it is a case of #3. Just call normally.
;; 5. If op is a special operator, just call normally.
;;
;; Since case 1 (op is a bound variable value) may actually end up being cases of 3 or 4
;; (regular function call semantics), we may end up with a lot of unnecessary partitioning.
;; Unfortunately, no one has built a typed Lisp yet.
(defn universal-call [obj args]
  (cond
    (startable/startable? obj) (startable/call-entry-point obj args)
    (instance? IFn obj) (apply obj args)
    :otherwise (throw (ex-info "Attempt to call object which doesn't implement Startable or IFn"
                        :type :runtime-error))))

(defn ^:suspending fcall [obj & args]
  (universal-call obj args))

(defn ^:suspending fapply
  ([flow] (universal-call flow ()))
  ([flow a1] (universal-call flow a1))
  ([flow a1 a2] (universal-call flow (cons a1 a2)))
  ([flow a1 a2 a3] (universal-call flow (cons a1 (cons a2 a3))))
  ([flow a1 a2 a3 & args]
   (universal-call flow (cons a1 (cons a2 (cons a3 (concat (butlast args) (last args))))))))

(s/def ::json (s/or
                :string string?
                :number number?
                :boolean boolean?
                :null nil?
                :map (s/map-of string? ::json)
                :array (s/and vector? (s/coll-of ::json))))
(s/def ::permit (s/or :keyword keyword? :json ::json :uuid uuid?))
(defn ^:suspending listen!
  [& {:keys [permit expires default]}]
  {:pre [(s/valid? (s/nilable ::permit) permit)
         (s/valid? (s/nilable #(instance? LocalDateTime %)) expires)]}
  (let [normalized-permit (if (keyword? permit)
                            (name permit) permit)]
    (if (not= normalized-permit permit)
      (log/warn (str "Keyword permit" permit " normalized to string. Please change this in your code.")))

    (signals/make-suspend-signal permit expires default)))

(defn ^:suspending block!
  "Suspends the current run until the provided child-run completes."
  [child-run & {:keys [expires default]}]
  (rt/attach-child-run! child-run)
  (listen! :permit (:id child-run) :expires expires :default default))

(defn respond!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (apply rt/add-responses! responses))

;;
;; Shortcut operators
;;
(defmacro !
  "Starts a run with the flow and given arguments.
  Returns the Run in :running or :complete state."
  [flow & args]
  `(rt/start! ~flow ~@args))

(defmacro <*
  [& {:keys [permit expires default]}]
  `(listen! ~@(rest &form)))

(defmacro <<!
  "The blocking operator - shortcut for block!"
  [child-run & {:keys [expires default]}]
  `(block! ~@(rest &form)))

(defmacro *>
  "Respond operator - shortcut for [[respond!]]"
  [& responses]
  `(respond! ~@(rest &form)))
