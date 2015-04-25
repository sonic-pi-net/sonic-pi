;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;
;; Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, and
;; distribution of modified versions of this work as long as this
;; notice is included.
;;++

(ns sonic-pi.synths.studio
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))


(do
  (without-namespace-in-synthdef
   (defsynth sonic-pi-mixer [in_bus 0 amp 1 safe-recovery-time 3 hpf_freq 0 hpf_pass_thru 1 lpf_freq 0 lpf_pass_thru 1 force_mono 0, invert_stereo 0]
     (let [l        (in in_bus 1)
           r        (in (+ in_bus 1))
           amp      (lag-ud amp 0 0.02)
           l        (* amp l)
           r        (* amp r)

           l        (select:ar hpf_pass_thru
                               [(hpf l hpf_freq)
                                l])
           r        (select:ar hpf_pass_thru
                               [(hpf r hpf_freq)
                                r])

           l        (select:ar lpf_pass_thru
                               [(lpf l lpf_freq)
                                l])
           r        (select:ar lpf_pass_thru
                               [(lpf r lpf_freq)
                                r])

           l   (select:ar force_mono
                          [l
                           (/ (+ l r) 2)])
           r   (select:ar force_mono
                          [r
                           (/ (+ l r) 2)])

           l2 (select:ar invert_stereo
                         [l
                          r])

           r2 (select:ar invert_stereo
                         [r
                          l])
           safe-snd (limiter [l2 r2] 0.99 0.01)]
       (replace-out 0 safe-snd)))

   (defsynth sonic-pi-basic_mixer [in_bus 0
                                   out_bus 0
                                   amp 1
                                   amp_slide 0.2
                                   amp_slide_shape 5
                                   amp_slide_curve 0]
     (let [amp      (varlag amp amp_slide  amp_slide_curve  amp_slide_shape)
           src      (in in_bus 2)
           src      (* amp src)]
       (out:ar out_bus src)))

   (defsynth sonic-pi-recorder
     [out-buf 0 in_bus 0]
     (disk-out out-buf (in in_bus 2)))


   (defsynth sonic-pi-sound_in [amp 1
                                amp_slide 0
                                pan 0
                                pan_slide 0
                                input 0
                                out_bus 0]
     (let [snd (sound-in 0)]
       (out out_bus (pan2 snd pan amp)))))


  (comment
    (core/save-synthdef sonic-pi-sound_in)
    (core/save-synthdef sonic-pi-mixer)
    (core/save-synthdef sonic-pi-basic_mixer)
    (core/save-synthdef sonic-pi-recorder)))
