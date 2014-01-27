(ns defpi.dom)

(defn by-id
  "Extract a dom element by id"
  [id]
  (.getElementById js/document id))

(defn by-class
  "Extract all dom elements matching class"
  [class]
  (.getElementsByClassName js/document class))

(defn set-html!
  "Set the inner html of a specific dom element el to s"
  [el s]
  (set! (.-innerHTML el) s))

(defn append-child!
  "Append the dom element c to element el as a child."
  [el c]
  (.appendChild el c))

(defn insert-before!
  "Incert the dom element c to before element el"
  [el c]
  (.insertBefore el c))

(defn get-html [el]
  (.-innerHTML el))
