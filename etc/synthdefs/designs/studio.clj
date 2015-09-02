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
   (defsynth sonic-pi-mixer [in_bus 0
                             pre_amp 1
                             pre_amp_slide 0.02
                             pre_amp_slide_shape 5
                             pre_amp_slide_curve 0
                             amp 1
                             amp_slide 0.02
                             amp_slide_shape 5
                             amp_slide_curve 0
                             safe-recovery-time 3
                             hpf 0
                             hpf_bypass 0

                             hpf_slide 0.02
                             hpf_slide_shape 5
                             hpf_slide_curve 0
                             lpf 135.5
                             lpf_bypass 0
                             lpf_slide 0.02
                             lpf_slide_shape 5
                             lpf_slide_curve 0
                             force_mono 0
                             invert_stereo 0
                             limiter_bypass 0
                             leak_dc_bypass 0]
     (let [l        (in in_bus 1)
           r        (in (+ in_bus 1))
           amp      (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pre_amp  (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
           hpf      (varlag hpf hpf_slide hpf_slide_curve hpf_slide_shape)
           lpf      (varlag lpf lpf_slide lpf_slide_curve lpf_slide_shape)
           lpf      (clip lpf 0 135.5)
           l        (* pre_amp l)
           r        (* pre_amp r)
           lpf_freq (midicps lpf)
           hpf_freq (midicps hpf)
           l        (select:ar hpf_bypass
                               [(overtone.live/hpf l hpf_freq)
                                l])
           r        (select:ar hpf_bypass
                               [(overtone.live/hpf r hpf_freq)
                                r])

           l        (select:ar lpf_bypass
                               [(overtone.live/lpf l lpf_freq)
                                l])
           r        (select:ar lpf_bypass
                               [(overtone.live/lpf r lpf_freq)
                                r])


           l        (select:ar force_mono
                               [l
                                (/ (+ l r) 2)])
           r        (select:ar force_mono
                               [r
                                l])

           l2       (select:ar invert_stereo
                               [l
                                r])

           r2       (select:ar invert_stereo
                               [r
                                l])

           l2       (select:ar limiter_bypass
                               [(limiter l2 0.99 0.01)
                                l2])

           r2       (select:ar limiter_bypass
                               [(limiter r2 0.99 0.01)
                                r2])



           l2       (select:ar leak_dc_bypass
                               [(leak-dc l2)
                                l2])

           r2       (select:ar leak_dc_bypass
                               [(leak-dc r2)
                                r2])

           snd      (* amp [l2 r2])

           safe-snd (clip2 snd 1)
           safe-snd (overtone.live/hpf safe-snd 10)
           safe-snd (overtone.live/lpf safe-snd 20500)]
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
                                amp_slide_shape 5
                                amp_slide_curve 0
                                pan 0
                                pan_slide 0
                                pan_slide_shape 5
                                pan_slide_curve 0
                                attack 0.01
                                decay 0
                                sustain 0
                                release 2
                                attack_level 1
                                decay_level 1
                                sustain_level 1
                                env_curve 2
                                cutoff 100
                                cutoff_slide 0
                                cutoff_slide_shape 5
                                cutoff_slide_curve 0

                                input 0
                                out_bus 0]
     (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           cutoff-freq (midicps cutoff)
           snd         (sound-in input)
           snd         (lpf snd cutoff-freq)
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* env snd) pan amp)))))


  (comment
    (core/save-synthdef sonic-pi-sound_in)
    (core/save-synthdef sonic-pi-mixer)
    (core/save-synthdef sonic-pi-basic_mixer)
    (core/save-synthdef sonic-pi-recorder)))
