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


(ns sonic-pi.traditional
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef
 (defsynth sonic-pi-pluck [note 52
                           amp 1
                           amp_slide 0
                           amp_slide_shape 1
                           amp_slide_curve 0
                           pan 0
                           pan_slide 0
                           pan_slide_shape 1
                           pan_slide_curve 0
                           attack 0
                           decay 0
                           sustain 0
                           release 1
                           attack_level 1
                           decay_level -1
                           sustain_level 1
                           env_curve 1
                           noise_amp 0.8
                           max_delay_time 0.125
                           pluck_decay 30
                           coef 0.3

                           out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge     2.5 ;; Given the filtering involved this synth is naturally quiet
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         freq          (midicps note)

         snd           (pluck     {:in           (* noise_amp (pink-noise))
                                   :trig         1
                                   :maxdelaytime max_delay_time
                                   :delaytime    (/ 1.0 freq)
                                   :decaytime    pluck_decay
                                   :coef         coef})

         env           (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         [new-l new-r] (pan2 (* amp-fudge snd env) pan amp)]
     (out out_bus [new-l new-r]))
   )


   (defsynth sonic-pi-blade
    "blade taken from Roger Allen's gist
    https://gist.githubusercontent.com/rogerallen/5992549/raw/2e4ed49bef990817e83981d812ab609e1b3bb901/violin.clj
    inspired by Sound On Sound April-July 2003 articles."
    [note 52
     note_slide 0
     note_slide_shape 1
     note_slide_curve 0
     amp 1
     amp_slide 0
     amp_slide_shape 1
     amp_slide_curve 0
     pan 0
     pan_slide 0
     pan_slide_shape 1
     pan_slide_curve 0
     attack 0
     decay 0
     sustain 0
     release 1
     attack_level 1
     decay_level -1
     sustain_level 1
     env_curve 1
     cutoff 100 ;; ~ 4000 Hz
     cutoff_slide 0
     cutoff_slide_shape 1
     cutoff_slide_curve 0
     vibrato_rate 6
     vibrato_rate_slide 0
     vibrato_rate_slide_shape 1
     vibrato_rate_slide_curve 0
     vibrato_depth 0.15
     vibrato_depth_slide 0
     vibrato_depth_slide_shape 1
     vibrato_depth_slide_curve 0
     vibrato_delay 0.5
     vibrato_onset 0.1
     out_bus 0]
    (let [decay_level   (select:kr (= -1 decay_level) [decay_level sustain_level])
          note          (varlag note note_slide note_slide_curve note_slide_shape)
          amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          amp-fudge     1.1
          pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
          vibrato_rate  (varlag vibrato_rate vibrato_rate_slide vibrato_rate_slide_curve vibrato_rate_slide_shape)
          vibrato_depth (varlag vibrato_depth vibrato_depth_slide vibrato_depth_slide_curve vibrato_depth_slide_shape)
          cutoff-freq   (midicps cutoff)

          ;; NOTE: this was the original vibrato implementation from Roger's code
          ;; freqv  (vibrato :freq freq :rate vibrato_rate :depth vibrato_depth :delay vibrato_delay :onset vibrato_onset)
          ;; freq   freqv
          ;; but this didn't seem to work on the ancient version of SuperCollider we use on the RPi
          freqv         (*
                         ;; delay before vibrato gets to full strength
                         (env-gen:kr (envelope [0 0 vibrato_depth] [vibrato_delay vibrato_onset]))
                         ;; actual frequency to add to the original pitch
                         (sin-osc:kr :freq vibrato_rate))

          ;; Calculate the vibrato in midi note (log frequency) then convert back
          freq          (midicps (+ note freqv))
          ;; the main osc for the violin
          saw           (saw freq)
          ;; a low-pass filter prior to our filter bank
          saw1          (lpf saw cutoff-freq)
          ;; the "formant" filters
          band1         (bpf saw1 300 (/ 3.5))
          band2         (bpf saw1 700 (/ 3.5))
          band3         (bpf saw1 3000 (/ 2))
          saw2          (+ band1 band2 band3)
          ;; a high-pass filter on the way out
          saw3          (hpf saw2 30)
          snd           (* amp-fudge saw3)
          env           (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
          ]
      (out out_bus (pan2 (* amp-fudge snd env) pan amp)))))

(comment
  (core/save-synthdef sonic-pi-pluck)
  (core/save-synthdef sonic-pi-synth_violin))
