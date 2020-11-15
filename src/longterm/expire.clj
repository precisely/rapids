(ns longterm.expire
  (:require [longterm.runstore :as rs]
            [longterm.run-loop :as rl]))

(defn expire-run! [run]
  {:pre [(rs/run-in-state? run :suspended)]}
  (let [{permit :permit,
         default :default}   (-> run :suspend)]
    (rl/continue! (:id run) permit default)))

