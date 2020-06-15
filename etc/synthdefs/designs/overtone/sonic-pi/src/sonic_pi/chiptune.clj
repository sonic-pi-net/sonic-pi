;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
;;
;; Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, and
;; distribution of modified versions of this work as long as this
;; notice is included.
;;++

(ns sonic-pi.chiptune
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef

  (defsynth sonic-pi-chiplead
    [note 60
     note_slide 0
     note_slide_shape 1
     note_slide_curve 0
     note_resolution 0.1
     amp 1
     amp_slide 0
     amp_slide_shape 1
     amp_slide_curve 0
     pan 0
     pan_slide 0
     pan_slide_shape 1
     pan_slide_curve 0
     attack 0
     sustain 0
     decay 0
     release 1
     attack_level 1
     decay_level -1
     sustain_level 1
     env_curve 1
     width 0
     out_bus 0]
    (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
          note        (round-down (varlag note note_slide note_slide_curve note_slide_shape) note_resolution)
          freq        (midicps note)
          amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          amp-fudge   0.8
          pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          width       (select:kr width [0.125
                                        0.25
                                        0.5])
          snd         (softclip (pulse freq width))
          env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
          snd         (* amp-fudge snd env)]

      (out out_bus (pan2 snd pan amp))))

  (defsynth sonic-pi-chipbass
    [note 60
     note_slide 0
     note_slide_shape 1
     note_slide_curve 0
     note_resolution 0
     amp 1
     amp_slide 0
     amp_slide_shape 1
     amp_slide_curve 0
     pan 0
     pan_slide 0
     pan_slide_shape 1
     pan_slide_curve 0
     attack 0
     sustain 0
     decay 0
     release 1
     attack_level 1
     decay_level -1
     sustain_level 1
     env_curve 1
     out_bus 0]
    (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
          note        (round-down (varlag note note_slide note_slide_curve note_slide_shape) note_resolution)
          freq        (midicps note)
          amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          amp-fudge   1
          pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          snd         (lin-lin (demand:ar (impulse:ar (* 32 freq)) 0 (dseq [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] INF)) 0 15 -1 1)
          env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
          snd         (* amp-fudge snd env)]

      (out out_bus (pan2 snd pan amp))))

  (defsynth sonic-pi-chipnoise
    [amp 1
     amp_slide 0
     amp_slide_shape 0
     amp_slide_curve 0
     pan 0
     pan_slide 0
     pan_slide_shape 1
     pan_slide_curve 0
     attack 0
     sustain 0
     decay 0
     release 1
     attack_level 1
     decay_level -1
     sustain_level 1
     env_curve 1
     freq_band 0
     freq_band_slide 0
     freq_band_slide_shape 1
     freq_band_slide_curve 0
     out_bus 0]
    (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
          amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          amp-fudge   0.8
          pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          ;; Frequency bands taken from information on http://nesdev.com/NESSOUND.txt
          freq_band   (floor (varlag freq_band freq_band_slide freq_band_slide_curve freq_band_slide_shape))
          noise_freq  (select:kr freq_band [1.0
                                            2.0
                                            4.0
                                            5.34
                                            8.0
                                            10.68
                                            16.0
                                            19.03
                                            25.4
                                            32.0
                                            42.71
                                            64.0
                                            128.0
                                            256.0
                                            512.0
                                            1024.0])
          snd         (softclip (lfd-clip-noise (* 220  noise_freq)))
          env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
          snd         (* amp-fudge snd env)]

      (out out_bus (pan2 snd pan amp))))

 (comment
   (core/save-synthdef sonic-pi-chipnoise)
   (core/save-synthdef sonic-pi-chiplead)
   (core/save-synthdef sonic-pi-chipbass)
 )
)
