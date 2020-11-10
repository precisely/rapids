(ns longterm.signals
  (:require [longterm.runstore :as rs]
            [longterm.util :as util])
  (:import (java.time LocalDateTime)))

;;;
;;; Unblock signal - signifies that a run has been unblocked
;;;                  without becoming the current run. The runloop
;;;                  should eval-next-runlet for this run
;;;
;(defrecord Unblock [run result]) ; signals that the run has changed
;(defn unblock-signal? [x] (instance? Unblock x))
;
;(defn make-unblock-signal
;  [run result]
;  {:pre [(rs/run-in-state? run :any)]}
;  (Unblock. run result))

;;
;; Continue Signal - signifies control should continue with the given run
;;
(defrecord Return []) ; signals that the run has changed
(defn return-signal? [x] (instance? Return x))

(defn make-return-signal
  []
  (Return.))

;;
;; Suspend Signal - indicates run should sleep
;;
(defrecord Suspend
  [permit                     ; the permit must match the value provided to continue!
   expires                    ; java.time.LocalDateTime
   default])                  ; if Suspend expires, this value is used

(defn suspend-signal? [x] (instance? Suspend x))

(defn suspending-operator? [x]
  (cond
    (var? x) (or (:suspending (meta x)) false)
    (qualified-symbol? x) (recur (find-var x))
    (symbol? x) (recur (resolve x))
    :else false))

(defn valid-suspend?
  [s]
  (and
    (instance? Suspend s)
    (or (-> s :expires nil?)
      (->> s :expires (instance? LocalDateTime)))
    (or (nil? (:default s)) (:expires s))))

(defn make-suspend-signal
  [permit expires default]
  {:post [(valid-suspend? %)]}
  (Suspend. permit expires default))

;;
;; signal? helper predicate - checks whether object is any of the above
;;
(defn signal? [x]
  (or (suspend-signal? x) (return-signal? x)))