(ns rapids.objects.flow-callable)

(defprotocol FlowCallable
  (entry-point [this]))