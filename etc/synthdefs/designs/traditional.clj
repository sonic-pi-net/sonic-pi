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


(ns sonic-pi.synths.traditional
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

(without-namespace-in-synthdef
 (defsynth sonic-pi-piano [note 52
                           note_slide 0
                           note_slide_shape 5
                           note_slide_curve 0
                           amp 1
                           amp_slide 0
                           amp_slide_shape 5
                           amp_slide_curve 0
                           pan 0
                           pan_slide 0
                           pan_slide_shape 5
                           pan_slide_curve 0
                           gate 1
                           vel 0.2
                           decay 0.2
                           release 0.2
                           hard 0.5
                           velhard 0.4
                           muffle 0.8
                           velmuff 0.8
                           velcurve 0.8
                           stereo_width 0.2
                           cutoff 0
                           cutoff_slide 0
                           cutoff_slide_shape 5
                           cutoff_slide_curve 0
                           res 0.2
                           res_slide 0
                           res_slide_shape 5
                           res_slide_curve 0

                           out_bus 0]
   (let [note          (varlag note note_slide note_slide_curve note_slide_shape)
         amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff-freq   (midicps cutoff)
         use-filter    (> cutoff 0)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         freq          (midicps note)
         vel           (clip vel 0 1)
         vel           (lin-lin vel 0 1 0 4)
         vel           (* vel 127)
         hard          (clip hard 0 1)
         hard          (lin-lin hard 0 1 -3 3)


         snd           (mda-piano {:freq freq
                                   :gate 1
                                   :vel vel
                                   :decay decay
                                   :release release
                                   :hard hard
                                   :velhard 0.8
                                   :muffle 0.8
                                   :velmuff 0.8
                                   :velcurve velcurve
                                   :stereo stereo_width
                                   :tune 0.5
                                   :random 0
                                   :stretch 0
                                   :sustain 0.1 })

         [snd-l snd-r] snd
         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])
         [new-l new-r] (balance2 snd-l snd-r pan amp)]
     (out out_bus [new-l new-r])
     (detect-silence snd 0.005 :action FREE) )

   )
 )

(comment
  (core/save-synthdef sonic-pi-piano))
