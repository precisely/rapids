(ns rapids.objects.address
  (:require [clojure.string :as string]
            [clojure.test :refer [is]]))

;; Address identifies a point in a flow.
;; While addresses can be generated for any point in the tree,
;; not every point is a valid location for a partition.
;; Addresses are meant to be human readable for debugging purposes
;; E.g., in
;; (deflow foo []       ; flow = myns/foo
;;   (if                ; point = [0]
;;     (test-something) ; point = [0,if,0]
;;     (then-clause)    ; point = [0,if,1]
;;     (do              ; point = [0,if,2]
;;        (fee          ; point = [0,if,2,do,0]
;;            (fi)      ; point = [0,if,2,do,0,fee,0] (the 0th arg of fee)
;;            :keyarg   ; point = invalid
;;            (fo))     ; point = [0,if,2,do,0,fee,2]
;;        (fum          ; point = [0,if,2,do,1]
;;          [           ; point = [0,if,2,do,1,fum,0]
;;            (x)       ; point = [0,if,2,do,1,fum,0,vec,0]
;;            (y)])))   ; point = [0,if,2,do,1,fum,0,vec,1]
;;   (baz               ; point = [1]
;;     {                ; point = [1,baz,0]
;;      :a              ; point = invalid
;;         (a)          ; point = [1,baz,0,map,1]
;;      :b              ; point = invalid
;;         (b)}))       ; point = [1,baz,0,map,2]
(declare to-string valid-point? simplify-if-symbol)

;; TODO: change Address to a custom class?
;;
;; cloverage instruments addresses in deflow bodies with extra keys, which causes
;; call-partition to fail. One solution may be to dissallow instrumenting Address
;; perhaps by changing it to a custom record which doesn't support IPersistentMap,
;; or by providing an implementation which prevents associng.
;;
(defrecord Address
  [flow                                                     ; Symbol
   point]                                                   ; Vector
  Object
  (toString [o] (str "<" (to-string o) ">")))

(defn address? [o] (instance? Address o))

(defn ->address
  [symbol & point]
  {:pre [(qualified-symbol? symbol) (valid-point? point)]}
  (Address.
    symbol
    (vec (map simplify-if-symbol point))))

(defn point-to-string [address]
  (letfn [(two-digit-num? [elt]
            (or (and (number? elt) (> elt 9))
              (if-let [str->num (try (Integer/parseInt (str elt)) (catch Exception e))]
                (> str->num 9))))
          (stringify-point-elt [elt]
            (if (two-digit-num? elt)
              (str "_" elt "_")
              (str elt)))]
    (str "$" (apply str (map stringify-point-elt (:point address))))))

(defn to-string
  [a]
  (str "$" (.getName (:flow a)) (point-to-string a)))

(defn child
  [address & point]
  {:pre [(instance? Address address)
         (valid-point? point)]}
  (assoc address :point (vec (concat (:point address) (map simplify-if-symbol point)))))

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

(defn resolved-flow
  [address]
  {:pre  [(instance? Address address)]
   :post [(or (nil? %) (.getName (type %)))]}
  (-> address :flow resolve var-get))

(defn valid-point?
  [elts]
  (if (and (every? #(or (keyword? %) (symbol? %) (number? %)) elts))
    true
    (println "Invalid point" elts)))

(defn simplify-if-symbol [x]
  (cond
    (= x '.) 'dot
    (qualified-symbol? x) (symbol (str (.getName x)))
    :otherwise x))

(defmethod print-method Address
  [o w]
  (print-simple
    (str "#<Address " (to-string o) ">")
    w))