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

(ns sonic-pi.synths.atmos
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

(without-namespace-in-synthdef
    (defsynth sonic-pi-dark_ambience
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

      cutoff 110
      cutoff_slide 0
      cutoff_slide_shape 1
      cutoff_slide_curve 0
      res 0.7
      res_slide 0
      res_slide_shape 1
      res_slide_curve 0

      detune1 12
      detune1_slide 0
      detune1_slide_shape 1
      detune1_slide_curve 0

      detune2 24
      detune2_slide 0
      detune2_slide_shape 1
      detune2_slide_curve 0

      noise 0
      ring 0.2
      room 70
      reverb_time 100
      out_bus 0
      ]
     (let [
           decay_level   (select:kr (= -1 decay_level) [decay_level sustain_level])
           note          (varlag note note_slide note_slide_curve note_slide_shape)
           amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge     1
           pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res           (lin-lin res 1 0 0 1)
           res           (varlag res res_slide res_slide_curve res_slide_shape)
           detune1       (varlag detune1 detune1_slide detune1_slide_curve detune1_slide_shape)
           detune2       (varlag detune2 detune2_slide detune2_slide_curve detune2_slide_shape)

           freq          (midicps note)
           freq2         (midicps (+ note detune1))
           freq3         (midicps (+ note detune2))
           cutoff-freq   (midicps cutoff)
           room          (max 0.1 (min 300 (abs room))) ;; stops synth killing scsynth
           env           (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

           pn            (* 0.005 (pink-noise))
           bn            (* 0.002 (brown-noise))
           wn            (* 0.002 (white-noise))
           cl            (* 0.001 (clip-noise))
           gn            (* 0.001 (gray-noise))
           src-noise     (select noise [pn bn wn cl gn])

           src1          (ringz src-noise freq ring)
           src2          (ringz src-noise freq2 ring)
           src3          (ringz src-noise freq3 ring)
           src           (g-verb (* env (mix [src1 src1 src2 src3])) room reverb_time)
           src           (tanh src)
           [src-l src-r] (rlpf (* amp-fudge env src) cutoff-freq res)
           src           (balance2 src-l src-r pan amp)
           ]
       (out out_bus src)))

    (defsynth sonic-pi-hollow
     [note 52
      note_slide 0
      note_slide_shape 1
      note_slide_curve 0

      pan 0
      pan_slide 0
      pan_slide_shape 1
      pan_slide_curve 0

      amp 1
      amp_slide 0
      amp_slide_shape 1
      amp_slide_curve 0

      attack 0
      decay 0
      sustain 0
      release 1
      attack_level 1
      decay_level -1
      sustain_level 1
      env_curve 1

      cutoff 90
      cutoff_slide 0
      cutoff_slide_shape 1
      cutoff_slide_curve 0
      res 0.99
      res_slide 0
      res_slide_shape 1
      res_slide_curve 0

      noise 1
      norm 0

      out_bus 0]
     (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
           note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           amp-fudge   15
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res         (lin-lin res 1 0 0 1)
           res         (varlag res res_slide res_slide_curve res_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

           pn          (* 1 (pink-noise))
           bn          (* 0.4 (brown-noise))
           wn          (* 0.5 (white-noise))
           cl          (* 0.2 (clip-noise))
           gn          (* 0.2 (gray-noise))
           src-noise   (select noise [pn bn wn cl gn])
           snd1        (bpf (* src-noise env) freq res)
           snd2        (* 0.5 (bpf (* src-noise env) (* freq 2) res))
           snd3        (* 0.125 (bpf (* src-noise env) (* freq 4) res))
           snd         (lpf (+ snd1 snd2 snd3) cutoff-freq)
           snd         (select norm [snd (normalizer snd)])]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))

    (defsynth sonic-pi-growl
     [out_bus 0

      note 52
      note_slide 0
      note_slide_shape 1
      note_slide_curve 0

      pan 0
      pan_slide 0
      pan_slide_shape 1
      pan_slide_curve 0

      amp 1
      amp_slide 0
      amp_slide_shape 1
      amp_slide_curve 0

      attack 0.1
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
]
     (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
           note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res         (lin-lin res 1 0 0 1)
           res         (varlag res res_slide res_slide_curve res_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           amp-fudge   3
           env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
           snd         (lpf (mix [(saw (* 0.25 freq)) (sin-osc (* 1.01 freq))]))
           snd         (pitch-shift snd 0.5 1 0 0.001)
           snd         (rlpf snd cutoff-freq res)
]
       (out out_bus (* env (pan2 (* amp-fudge snd) pan amp)))))

    (comment
      (core/save-synthdef sonic-pi-dark_ambience)
      (core/save-synthdef sonic-pi-hollow)
      (core/save-synthdef sonic-pi-growl)))
