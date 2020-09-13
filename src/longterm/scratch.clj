
(declare fb fb2 fb3 fb4)

(do (let [result (fb) result (fb2)] (fb3)) (fb4))


;; This is a scratchpad for thinking through the algorithm
(declare deflow place-orders order-tests items place-order
  chat add-thunk wait-for-results with-continuation analyze-result
  Flow log user-symptoms-dialog tests-from-symptoms call-continuation user-testing-dialog wait-for-event order order-event-id)

(deflow order-tests [items]
  (if (count items)
    (let [order (place-order items)] ; normal fn
      (chat "I ordered some items!")
      (let [result (wait-for-results order)] ; flow, storing return in result
        (chat "The results from order %s are ready:"
          (:id order) (analyze-result result))))
    (do (chat "No items detected - would you like to select again?")
        (order-tests (tests-from-symptoms (user-symptoms-dialog)))))

  ; this logging expression demonstrates how code after a branch is handled
  (log "exit order-tests"))

(deflow wait-for-results [order]
  (wait-for-event (order-event-id order)))  ; wait-for-event is a system primitive

(deflow user-symptoms-dialog []
  ; dialog with the user and determine what tests to order
  )

#Flow{:name        'order-tests
      :entry-point (fn [items] (call-continuation order-tests 0 :items items))
      :continuations
                   [; CONTINUATION 0
                    (fn [& {keys: [items]}]
                      (if (count items)
                        (let [order (place-order items)]
                          (chat "I ordered some tests!")
                          (add-thunk ['order-tests 1 {:items items :order order} 'result]
                            (wait-for-results order)))
                        (do
                          (chat "No items detected - would you like to select again?")
                          (add-thunk ['order-tests 2 {:items items} 'user-symptoms-dialog-result__1941__auto__]
                            (user-testing-dialog)))))

                    ; CONTINUATION 1
                    (fn [& {:keys [items order result]}]
                      (chat "The results from order %s are ready:"
                        (:id order) (analyze-result result))
                      (call-continuation order-tests 3 :items items :order order :result result))

                    ; CONTINUATION 2
                    (fn [& {:keys [items user-symptoms-dialog-result__1941__auto__]}]
                      (with-continuation ['order-tests 3 {} nil]
                        (order-tests (tests-from-symptoms user-symptoms-dialog-result__1941__auto__)))

                      ; CONTINUATION 3
                      (fn [& {:keys [items]}]
                        (log "exit order tests"))]}

