(ns rapids.runtime.calling
  (:require [rapids.runtime.runlet :refer [current-address]]
            [rapids.objects.address :refer [address-module address-major-version]]
            [rapids.objects.startable :as startable])
  (:import (clojure.lang IFn)))

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
    ;; startable
    (startable/startable? obj)
    (let [caddr (current-address)]
      (if (some-> caddr address-module (= (startable/module obj)))
        ;; same
        (startable/begin obj args (address-major-version caddr))
        (let [current-requirements (-> caddr :flow resolve :requirements)
              required-version     (get current-requirements (startable/module obj))]
          (startable/begin obj args required-version))))

    ;; simple function
    (instance? IFn obj) (apply obj args)
    :otherwise (throw (ex-info "Attempt to call object which doesn't implement Startable or IFn"
                        {:type   :runtime-error
                         :object obj}))))

(defn ^:suspending fcall [obj & args]
  (universal-call obj args))

(defn ^:suspending fapply
  ([flow] (universal-call flow ()))
  ([flow a1] (universal-call flow a1))
  ([flow a1 a2] (universal-call flow (cons a1 a2)))
  ([flow a1 a2 a3] (universal-call flow (list* a1 a2 a3)))
  ([flow a1 a2 a3 & args]
   (universal-call flow (list* a1 a2 a3 (concat (butlast args) (last args))))))
