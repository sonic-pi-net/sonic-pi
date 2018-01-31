;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;
;; Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, and
;; distribution of modified versions of this work as long as this
;; notice is included.
;;++

(ns sonic-pi.synths.tech
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

(without-namespace-in-synthdef
  (defsynth sonic-pi-tech_saws [note 52
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
                                cutoff 130
                                cutoff_slide 0
                                cutoff_slide_shape 1
                                cutoff_slide_curve 0
                                res 0.7
                                res_slide 0
                                res_slide_shape 1
                                res_slide_curve 0
                                out_bus 0]
            (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
                  note (varlag note note_slide note_slide_curve note_slide_shape)
                  amp (varlag amp amp_slide amp_slide_curve amp_slide_shape)
                  amp-fudge 0.9
                  pan (varlag pan pan_slide pan_slide_curve pan_slide_shape)
                  cutoff (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
                  res (lin-lin res 1 0 0 1)
                  res (varlag res res_slide res_slide_curve res_slide_shape)
                  freq (midicps note)
                  cutoff-freq (midicps cutoff)
                  snd-fn (fn [freq]
                           (let [tune (ranged-rand 0.99 1.01)]
                             (-> (lf-saw (* freq tune))
                                 (delay-c 0.005 (ranged-rand 0.0001 0.01)))))
                  hi-saws (splay (repeatedly 5 #(snd-fn freq)))
                  lo-saws (splay (repeatedly 5 #(snd-fn (/ freq 2))))
                  noise (pink-noise)
                  output (+ (* 0.65 hi-saws) (* 0.85 lo-saws) (* 0.07 noise))
                  output (clip2 output 0.55)
                  output (normalizer (rlpf output cutoff-freq res))
                  env (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
                  output (free-verb output :room 1.8 :mix 0.45)
                  output (* amp-fudge env output)
                  output (pan2 output pan amp)]
              (out out_bus output)))

 (comment
   (core/save-synthdef sonic-pi-chipnoise)))
