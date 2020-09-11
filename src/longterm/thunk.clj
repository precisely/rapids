(ns longterm.thunk)


;;;; Thunks

;;; thunks are functions
(defrecord ThunkId [name hash n])
(defrecord ThunkDef [params body])
