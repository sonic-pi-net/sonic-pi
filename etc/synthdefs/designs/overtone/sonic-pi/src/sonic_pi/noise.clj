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

(ns sonic-pi.noise
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef

 (defsynth sonic-pi-bnoise
   [amp 1
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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.2
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (brown-noise) cutoff-freq res)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-pnoise
   [amp 1
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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp_fudge   3
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (pink-noise) cutoff-freq res)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (* amp_fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-gnoise
   [amp 1
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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp_fudge   1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (gray-noise) cutoff-freq res)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (* amp_fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-noise
   [amp 1
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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.9
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (white-noise) cutoff-freq res)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-cnoise
   [amp 1
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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.6
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (clip-noise) cutoff-freq res)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))

 (comment
   (core/save-synthdef sonic-pi-noise)
   (core/save-synthdef sonic-pi-pnoise)
   (core/save-synthdef sonic-pi-bnoise)
   (core/save-synthdef sonic-pi-gnoise)
   (core/save-synthdef sonic-pi-cnoise)
   )
 )
