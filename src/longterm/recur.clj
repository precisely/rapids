(ns longterm.recur)

;;
;; Macros used for loop/recur partitioning
;;

(def ^:dynamic *recur-binding-point* nil)
(def ^:dynamic *tail-position*
  "Can be nil, false or true; nil = undefined; false = non-tail; true=tail"
  nil)

(defmacro with-tail-position [[state] & body]
  "Allows partitioning functions to determine whether they are executing
  within a tail context. Note that the body argument represents calls to
  partitioning functions, not user-land flows.

  state is boolean expression which sets *tail-position*

  Nested with-tail-position contexts set *tail-position* as follows:

  state may be an expression which evaluates to: truthy, falsey or :reset

  :reset => sets *tail-position* to nil
  truthy => sets *tail-position* to true if *tail-position* is true or nil
  falsey => sets *tail-position* to false"
  `(let [state# ~state]
     (binding [*tail-position* (cond
                                 (not state#) false
                                 (nil? *tail-position*) state#
                                 (= state# :reset) nil
                                 :else (and state# *tail-position*))]
       ~@body)))

(defmacro with-binding-point
  "Establishes a flow binding point to be used by partition-recur-expr.
  The body argument represents calls to partitioning functions, not user-land flows."
  [[address params] & body]
  `(binding [*recur-binding-point* {:address ~address :params ~params}]
     ~@body))
