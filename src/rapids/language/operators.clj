(ns rapids.language.operators
  (:require [rapids.runtime.core :as rt]
            [rapids.objects.signals :as signals]
            [rapids.objects.startable :as callable]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [rapids.objects.startable :as startable])
  (:import (java.time LocalDateTime)))

;;
;; Callable methods
;;
(defn ^:suspending fcall [obj & args]
  (startable/call-entry-point obj args))

(defn ^:suspending fapply
  ([flow] (startable/call-entry-point flow ()))
  ([flow a1] (startable/call-entry-point flow a1))
  ([flow a1 a2] (startable/call-entry-point flow (cons a1 a2)))
  ([flow a1 a2 a3] (startable/call-entry-point flow (cons a1 (cons a2 a3))))
  ([flow a1 a2 a3 & args]
   (startable/call-entry-point flow (cons a1 (cons a2 (cons a3 (concat (butlast args) (last args))))))))

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