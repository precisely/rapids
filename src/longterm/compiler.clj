(ns longterm.compiler
  (:import
    java.security.MessageDigest
    java.math.BigInteger))

(declare compile-body compile-expr compile-list-expr compile-flow-expr
  compile-if-expr compile-let-expr compile-fn-expr compile-fncall-expr
  compile-loop-expr compile-special-expr compile-recur-expr extract-bindings md5 md5-hash)

(defn compile-body
  "Compiles a list of expressions (body), returning [continuationdef*]"
  [continuation-id body]
  (loop [expr body]
    (if (nil? expr)
      [thunkdefs body]
      (let [new-thunkdefs (compile-expr
                            (contains-flow-breaker? expr)
                            (longterm.thunk/with-thunk []
                              (recur thunkdefs (rest body))))))))

; compile-expr => [continuations remainder-expr]
(defn compile-expr
  [flow expr]
  (if (list? expr)
    (compile-callable flow expr)
    `[~expr]))

(defn compile-list-expr
  [flow expr]
  (let [op (first expr)]
    (cond
      (instance? Flow op) (compile-flow-expr flow expr)
      (fn? op) (compile-fncall-expr flow expr)
      (special-symbol? op) (compile-special-expr flow expr))))

(defn compile-special-expr
  [flow expr]
  (let [op (first expr)]
    (case op
      if (compile-if-expr flow expr)
      let (compile-let-expr flow expr)
      fn (compile-fn-expr flow expr)
      loop (compile-loop-expr flow expr)
      recur (compile-recur-expr flow expr)
      (throw "%s not recognized. Unable to compile special form: %s at " op expr))))

;(deflow heart-risk-assessment []
;  (let* [
;         biometrics (obtain "Intake.basic-biometrics")
;         cardiac-intake (obtain "Intake.cardiac" :max-age (months 3))
;         cardiac-fh (obtain "FamilyHistory.cardiac" :max-age (years 5))
;         symptoms (obtain "FamilyHistory.cardiac" :max-age (months 1))
;         qrisk (calculate-qrisk biometrics cardiac-fh cardiac-intake)
;         home-tests (make-home-visit :home-tests)
;         follow-up-home-tests (make-home-visit :home-tests)
;         ecg-clinic (make-clinic-visit :ecg)]
;    (schedule home-tests :blood-pressure)
;    (util/range-case
;      ((between? (biometrics :age) 20 40)
;       (if (contains-any cardiac-fh '(:high-cholesterol :dyslipidemia)))
;       (schedule home-tests :lipids)))
;    ((between? (biometrics :age) 41 75)
;     (schedule home-tests :lipids)
;     (if (> qrisk 5)
;       (schedule home-tests :ankle-brachial-index :hs-CRP :apoB))
;     (if (between? qrisk 7.5 20)
;       (schedule home-tests :coronary-artery-calcium))))
;  (if (:chest-pain symptoms)
;    (let [angina (obtain "Intake.angina" :max-age (months 1)))
;    (if (angina :has-condition)
;      (schedule home-tests :troponin)
;      (schedule clinic-tests :electrocardiogram)))
;  (on-result (result (home-tests :blood-pressure))
;    (if (or (> (result :systolic) 140) (>= (result :diasystolic) 90))
;      (schedule follow-up-home-tests :fasting-plasma-glucose :blood-count-full :creatine-with
;
;        ;; Use this macro at the top of a longterm code:
;        ;; (ns foo.bar (:require [longterm.core :as lt]))
;        ;; (lt/uselt)
;        (defmacro uselt
;          "Imports all the longterm macros and functions"
;          ([ns-symbol]
;           `(~'ns ~ns-symbol (:require [longterm.core :refer-macros [~'defp]])))
;
;          ([]
;           (println ":name &env" (if &env (:name &env) "&env unbound"))
;           (println "ns-name *ns*" (if *ns* (ns-name *ns*) "*ns* unbound"))
;           (cond
;             *ns* `(uselt ~(symbol (ns-name *ns*)))
;             &env `(uselt ~(symbol (:name &env)))
;             :else (throw "Unable to determine current namespace"))))


(defn md5 [^String s]
          (let [algorithm (MessageDigest/getInstance "MD5")
                raw (.digest algorithm (.getBytes s))]
            (format "%032x" (BigInteger. 1 raw))))
(defn md5-hash [s] (md5 (pr-str s)))


(defn bindings-from-args
  "given an argument vector, "
  ([args] (bindings-from-args args []))
  ([args bindings]
   (let [arg (first args)]
     (cond
       (= arg '&) (bindings-from-args (rest args) bindings)
       (map? arg) (bindings-from-args (rest args) (concat bindings (:keys args)))
       (= arg nil) bindings
       :else (throw (str "Unexpected argument " arg))))))
