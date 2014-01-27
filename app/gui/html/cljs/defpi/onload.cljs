(ns defpi.onload
  (:require [defpi.ws :refer [add-ws-handlers]]
            [defpi.canvas]))

(set! (.-onload js/window)
      (fn []
        (add-ws-handlers)))
