(ns longterm.address
  (:require [clojure.string :as string]))

;; Address identifies a point in a flow.
;; While addresses can be generated for any point in the tree,
;; not every point is a valid location for a continuation.
;; Addresses are meant to be human readable for debugging purposes
;; E.g., in
;; (deflow foo []       ; flow = 'foo
;;   (if                ; point = "0"
;;     (test-something) ; point = "0/if/0"
;;     (then-clause)    ; point = "0/if/1"
;;     (do              ; point = "0/if/2"
;;        (fee          ; point = "0/if/2/do/0"
;;            (fi)      ; point = "0/if/2/do/0/fee/0" (the 0th arg of fee)
;;            :keyarg   ; point = invalid
;;            (fo))     ; point = "0/if/2/do/0/fee/2
;;        (fum          ; point = "0/if/2/do/1"
;;          [           ; point = "0/if/2/do/1/fum/0"
;;            (x)       ; point = "0/if/2/do/1/fum/0/[]/0"
;;            (y)])))   ; point = "0/if/2/do/1/fum/0/[]/1"
;;   (baz               ; point = "1"
;;     {                ; point = "1/baz/0"
;;      :a              ; point = invalid
;;         (a)          ; point = "1/baz/0/{}/1"
;;      :b              ; point = invalid
;;         (b)}))       ; point = "1/baz/0/{}/2"
(defrecord Address
  [flow                                 ; Symbol
   point]                               ; Vector
  Object
  (toString [o] (str "#<Address " flow ":" (string/join "/" point) ">")))

(defn create
  [symbol & points]
  (Address. symbol (vec points)))

(defmethod print-method Address
  [o w]
  (print-simple
    (str "#<Address " (:flow o) ":" (string/join "/" (:point o)) ">")
    w))

(defn to-string
  [a]
  (str (:flow a) ":" (string/join "/" (:point a))))

(defn from-string
  [s]
  (let [[matched? name pointdefs] (re-find #"^([^\:\s]*)\:([^:\s]*)" s)]
    (if matched?
      (let [points (map #(if (re-find #"^\d" %) (int %) %) (string/split pointdefs "/"))]
        (Address. (symbol name) points))
      (throw (Exception. (format "Expecting Address string definition, but received %s" s))))))

(defn child
  [address & point-elts]
  (assert (instance? Address address))
  (assoc address :point (vec (concat (:point address) point-elts))))

(defn increment
  ([address] (increment address 1))

  ([address step]
   (let [point (:point address)
         last  (last point)]
     (assoc address :point (conj (pop point)
                             (+ step last))))))

(defn resolve-continuation [address]
                           (let [[flow point] address]
                             (-> (var-get (resolve flow)) :continuations point)))

