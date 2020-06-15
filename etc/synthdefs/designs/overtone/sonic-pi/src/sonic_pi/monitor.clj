;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
;;
;; Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, and
;; distribution of modified versions of this work as long as this
;; notice is included.
;;++

(ns sonic-pi.monitor
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef

 (defsynth sonic-pi-amp_stereo_monitor [bus 0 smoothness 0.1]
   (let [tr  (impulse:kr 5)
         left (a2k (lag (abs (in:ar bus)) smoothness))
         right (a2k (lag (abs (in:ar (+ bus 1))) smoothness))]
     (send-reply tr "/sonic-pi/amp" [left right] bus))))


  (comment
    (core/save-synthdef sonic-pi-amp_stereo_monitor)
)
