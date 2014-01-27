(ns defpi.onload
  (:require [defpi.ws :refer [add-ws-handlers]]))

(set! (.-onload js/window)
      (fn []
        (add-ws-handlers)))
