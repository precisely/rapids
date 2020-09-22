(ns longterm.continuation_set
  (:use longterm.thunk)
  (:require
    [longterm.continuation_def :as cdef]
    [longterm.thunk :as thunk]))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add continue create combine)

;;; ContinuationSet

;; Partitioning functions sometimes need to leave undefined the precise address
;; of a partition connects to until a later point in the computation.
;; For example, when processing an series of arguments, it isn't clear
;; until the end of the argument list is reached whether the next address
;; should be the next partition in the arglist or the partition containing
;; the function call.
;; The ContinuationSet leaving function definitions unresolved as follows:

(defn create [] {})

(defn unresolved? [cset]
  "Lazy list of unresolved ContinuationDefs"
  (for [[_ v] cset :when (not (cdef/resolved? v))] v))

(defn add
  ([cset address params body & {:keys [result-key]}]
   (assoc-in cset [:bodies address]
     (if (fn? body)
       (fn [next-addr]
         (let [resolved-body (body next-addr)]
           `(fn [& {:keys ~params}] ~@resolved-body)))
       (do
         `(fn [& {:keys ~params}] ~@body))))))

(defn get-continuation-def [cset address]
  "Returns an expression which defines a continuation function"
  (let [body (get-in cset [:bodies address])]
    (assert (not (fn? body))
      (format "Cannot construct continuation definition for %s body at %s."
        (if (nil? body) "undefined" "unresolved")
        address))
    `(fn [{:keys, (get-in cset [:params address])}], @(get-in cset [:bodies address]))))

(defn record-connection
  "Used when for recording a connection when a resolved body is added"
  [cset src dest]
  (assoc cset :connections (conj (:connections cset) [src dest])))

(defn connect
  "Connects a single src-address to a single dest-address. If params
  and body are provided, they will be used to define a continuation
  at dest-address.

  cset - ContinuationSet
  src-address - address
  dest-address - address
  params - dest-address params
  body - function which takes address as argument and returns a body connecting
         to that address
  result-key - param to hold the results of body in the next continuation"
  ([cset src-address dest-address params body]
   (connect cset src-address dest-address params body nil))

  ([cset src-address dest-address params body]
   (-> cset
     (add dest-address params body)
     (connect src-address dest-address)))

  ([cset src-address dest-address]
   (let [src-body-generator (get-in cset [:bodies src-address])]
     (assert (fn? src-body-generator))
     (-> cset
       (assoc-in [:bodies src-address] (src-body-generator dest-address))
       (assoc :graph (conj (:graph cset) [src-address dest-address]))))))

(defn continue
  "Connects all unresolved addresses in the first ContinuationSet (cset1) to dest-address
  in the second ContinuationSet (cset2), returning a combined ContinuationSet."
  ([cset1 dest-address params body]
   (continue cset1 dest-address
     (-> (create)
       (add dest-address params body))))

  ([cset1 dest-address cset2]
   (reduce (fn [cset src-address]
             (connect cset src-address dest-address))
     (combine cset1 cset2)
     (unresolved-addresses cset1))))

(defn combine
  "Combines all of the continuations and unresolved addresses in two ContinuationSets"
  [cset1 cset2]
  (assoc cset1
    :bodies (merge (:bodies cset1) (:bodies cset2))
    :params (merge (:params cset1) (:params cset2))
    :connections (concat (:connections cset1) (:connections cset2))))

