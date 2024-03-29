(ns rapids.objects.signals
  (:import (java.time LocalDateTime)))


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
    (nil? x) false
    (var? x) (or (:suspending (meta x)) false)
    (qualified-symbol? x) (recur (try (find-var x) (catch Exception _)))
    (symbol? x) (recur (try (resolve x) (catch Exception _)))
    :else false))

(defn valid-suspend?
  [s]
  (and
    (instance? Suspend s)
    (or (-> s :expires nil?)
        (->> s :expires (instance? LocalDateTime)))))

(defn make-suspend-signal
  [permit expires default]
  {:post [(valid-suspend? %)]}
  (Suspend. permit expires default))

(defrecord BindingChangeSignal [pop? stack-fn result dynamics])

(defn binding-change-signal? [o] (and o (instance? BindingChangeSignal o)))
