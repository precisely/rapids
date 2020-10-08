(ns longterm.address
  (:require [clojure.string :as string]
            [clojure.test :refer [is]]))

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
(declare to-string valid-point?)
(defrecord Address
  [flow                                                     ; Symbol
   point]                                                   ; Vector
  Object
  (toString [o] (str "<" (to-string o) ">")))

(defn address? [o] (instance? Address o))
(defn create
  [symbol & point]
  {:pre [(is (qualified-symbol? symbol) "Address must flow must be a fully qualified symbol")
         (is (valid-point? point) "Expecting varargs containing symbols or numbers")]}
  (Address. symbol (vec point)))

(defn to-string
  [a]
  (str (:flow a) ":" (string/join ";" (:point a))))

;(defmethod print-method Address
;  [o w]
;  (print-simple
;    (str "<" (to-string o) ">")
;    w))

(defn from-string
  [s]
  (let [[matched? name pointdefs] (re-find #"^([^;:\s]*)\:([^:\s]*)" s)]
    (if matched?
      (let [points (map #(if (re-find #"^\d" %) (int %) (symbol %)) (string/split pointdefs ";"))]
        (apply create (symbol name) points))
      (throw (Exception. (format "Expecting Address string definition, but received %s" s))))))

(defn child
  [address & point]
  {:pre [(is (instance? Address address) "First arg to address/child should be an address")
         (is (valid-point? point) "Varargs of address/child should be symbols or numbers")]}
  (assoc address :point (vec (concat (:point address) point))))

(defn increment
  ([address] (increment address 1))

  ([address step]
   {:pre [(instance? Address address)
          (number? step)
          (-> address :point last number?)]}
   (let [point (:point address)
         last (last point)]
     (assoc address :point (conj (pop point)
                                 (+ step last))))))

(defn resolve-continuation
  [address]
  (let [[flow point] address]
    (-> (var-get (resolve flow)) :continuations point)))

(defn resolved-flow
  [address]
  {:pre  [(instance? Address address)]
   :post [(or (nil? %) (.getName (type %)))]}
  (-> address :flow resolve var-get))

(defn valid-point?
  [elts]
  (and (every? #(or (symbol? %) (number? %)) elts)))