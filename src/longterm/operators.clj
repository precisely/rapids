(ns longterm.operators
  (:require [longterm.run-context :as rc]
            [longterm.run-loop :as rl]
            [longterm.flow :as flow]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s])
  (:import (java.time LocalDateTime)))

;;
;; Flow methods
;;
(defn ^:suspending fcall [flow & args]
  (flow/entry-point flow args))

(defn ^:suspending fapply
  ([flow] (flow/entry-point flow ()))
  ([flow a1] (flow/entry-point flow a1))
  ([flow a1 a2] (flow/entry-point flow (cons a1 a2)))
  ([flow a1 a2 a3] (flow/entry-point flow (cons a1 (cons a2 a3))))
  ([flow a1 a2 a3 & args]
   (flow/entry-point flow (cons a1 (cons a2 (cons a3 (concat (butlast args) (last args))))))))

(s/def ::json (s/or
                :string string?
                :number number?
                :boolean boolean?
                :null nil?
                :map (s/map-of string? ::json)
                :array (s/and vector? (s/coll-of ::json))))
(s/def ::permit (s/or :keyword keyword? :json ::json))
(defn ^:suspending listen!
  [& {:keys [permit expires default]}]
  {:pre [(s/valid? (s/nilable ::permit) permit)
         (s/valid? (s/nilable #(instance? LocalDateTime %)) expires)]}
  (let [normalized-permit (if (keyword? permit)
                            (name permit) permit)]
    (if (not= normalized-permit permit)
      (log/warn (str "Keyword permit" permit " normalized to string. Please change this in your code.")))

    (rc/set-listen! normalized-permit, expires, default)))

(defn ^:suspending block!
  "Suspends the current run and starts a run in :block mode"
  [child-run & {:keys [expires default]}]
  (rc/set-blocker! child-run expires default))

(defn ^:suspending redirect!
  "transfers execution to child-run"
  [child-run & {:keys [expires default]}]
  (rc/set-redirect! child-run expires default))

(defn respond!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (apply rc/add-responses! responses))

;;
;; Shortcut operators
;;
(defmacro !
  "Starts a run with the flow and given arguments.
  Returns the Run in :suspended or :complete state."
  [flow & args]
  `(rl/start! ~flow ~@args))

(defmacro <*
  [& {:keys [permit expires default]}]
  `(listen! ~@(rest &form)))

(defmacro <<!
  "The blocking operator - shortcut for block!"
  [child-run & {:keys [expires default]}]
  `(block! ~@(rest &form)))

(defmacro >>
  "Redirect operator - shortcut for [[redirect]]"
  [child-run & {:keys [expires default]}]
  `(redirect! ~@(rest &form)))

(defmacro *>
  "Respond operator - shortcut for [[respond!]]"
  [& responses]
  `(respond! ~@(rest &form)))
