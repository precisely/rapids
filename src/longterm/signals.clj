(ns longterm.signals
  (:import (java.time LocalDateTime)))

;;
;; Return Signal - signifies control should return to the given run
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