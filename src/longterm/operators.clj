(ns longterm.operators
  (:require [longterm.run-context :as rc]
            [longterm.run-loop :as rl]
            [longterm.flow :as flow]))

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

(defn ^:suspending listen!
  [& {:keys [permit expires default]}]
  (rc/set-listen! permit, expires, default))

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
