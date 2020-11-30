(ns longterm.expire
  (:require [longterm.run-loop :as rl]
            [longterm.run :as r]))

(defn expire-run! [run]
  {:pre [(r/run-in-state? run :suspended)]}
  (let [{permit :permit,
         default :default}   (-> run :suspend)]
    (rl/continue! (:id run) permit default)))

