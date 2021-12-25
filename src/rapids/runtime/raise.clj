(ns rapids.runtime.raise
  (:require [rapids.runtime.calling :refer [fcall]]
            [rapids.runtime.runlet :as rl]                  ; required to get deflow to compile
            [rapids.language.flow :refer [deflow]]
            [rapids.objects.address :refer [->address]]
            [rapids.objects.interruptions :refer [interruption?]]
            [rapids.runtime.globals :refer [*attempts*]]))

;; TODO: determine why rapids.runtime.runlet is required to get this to compile
;;       for some reason, the (fcall...) forms below cause a compiler error of
;;       ClassNotFoundException where :message is rapids.runtime.runlet
(deflow raise
  "Raises an interruption."
  [interrupt]
  {:pre [(interruption? interrupt)]}
  ;; TODO: check that current-run exists and that cache is bound
  (loop [attempts *attempts*]
    (if (empty? attempts)
      (throw (ex-info "Unhandled interruption"
               {:type         :input-error
                :interruption interrupt})))
    (let [[attempt & attempts] attempts
          handler (first (filter #(= (:name %) (:name interrupt)) (:handlers attempt)))]
      (if handler
        (let [finally (:finally attempt)
              result (fcall (:flow handler) interrupt)]
          (if finally (fcall finally))
          result)
        (recur (rest attempts))))))

;; it's a bit hacky to get the entry point this way, but run-loop/interrupt! needs it
(def raise-partition-fn-address
  "The entry partition-fn address of the raise flow"
  (->address `raise 0))
