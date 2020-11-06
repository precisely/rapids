(ns longterm.operators
  (:require [longterm.run-context :as rc]
            [longterm.run-loop :as rl]))

;;
;; Flow methods
;;
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
  (rc/add-responses! responses))

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

(defmacro <!
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
