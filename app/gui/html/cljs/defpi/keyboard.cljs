(ns defpi.keyboard
  (:require
   [defpi.ws :as ws]
   [goog.events :as events]
   [goog.events.EventType]))


(def keyword->event-type
  {:keyup goog.events.EventType.KEYUP
   :keydown goog.events.EventType.KEYDOWN
   :keypress goog.events.EventType.KEYPRESS
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :mousedown goog.events.EventType.MOUSEDOWN
   :mouseup goog.events.EventType.MOUSEUP
   :mouseover goog.events.EventType.MOUSEOVER
   :mouseout goog.events.EventType.MOUSEOUT
   :mousemove goog.events.EventType.MOUSEMOVE
   :focus goog.events.EventType.FOCUS
   :blur goog.events.EventType.BLUR})

(defn charcode->char
  [code]
  (js/String.fromCharCode code))

(events/listen js/document (keyword->event-type :keypress)
               (fn [e]
                 (let [code (charcode->char (.-charCode e))]
                   (.send ws/ws {:cmd "event"
                                 :type :keypress
                                 :val code}))))
