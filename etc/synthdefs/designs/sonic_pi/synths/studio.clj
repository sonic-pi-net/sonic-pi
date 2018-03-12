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
                             pre_amp_slide_shape 1
                             pre_amp_slide_curve 0
                             amp 1
                             amp_slide 0.02
                             amp_slide_shape 1
                             amp_slide_curve 0
                             hpf 22
                             hpf_bypass 0
                             hpf_slide 0.02
                             hpf_slide_shape 1
                             hpf_slide_curve 0
                             lpf 136
                             lpf_bypass 0
                             lpf_slide 0.02
                             lpf_slide_shape 1
                             lpf_slide_curve 0
                             force_mono 0
                             invert_stereo 0
                             limiter_bypass 0
                             leak_dc_bypass 0
                             out_bus 0]
     (let [l        (+ (in out_bus) (in in_bus))
           r        (+ (in (+ out_bus 1)) (in (+ in_bus 1)))
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
       (replace-out out_bus safe-snd)))

   (defsynth sonic-pi-basic_mixer [in_bus 0
                                   out_bus 0
                                   amp 1
                                   amp_slide 0.2
                                   amp_slide_shape 1
                                   amp_slide_curve 0]
     (let [amp      (varlag amp amp_slide  amp_slide_curve  amp_slide_shape)
           src      (in in_bus 2)
           src      (* amp src)]
       (out:ar out_bus src)))

   (defsynth sonic-pi-recorder
     [out-buf 0 in_bus 0]
     (disk-out out-buf (in in_bus 2)))


   (defsynth sonic-pi-live_audio_mono [amp 1
                                       amp_slide 0
                                       amp_slide_shape 1
                                       amp_slide_curve 0
                                       pan 0
                                       pan_slide 0
                                       pan_slide_shape 1
                                       pan_slide_curve 0
                                       input 1
                                       out_bus 0]
     (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           snd         (sound-in (- input 1))]
       (out out_bus (pan2 snd pan amp))))

   (defsynth sonic-pi-live_audio_stereo [amp 1
                                amp_slide 0
                                amp_slide_shape 1
                                amp_slide_curve 0
                                pan 0
                                pan_slide 0
                                pan_slide_shape 1
                                pan_slide_curve 0
                                input 1
                                out_bus 0]
     (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           snd-l       (sound-in (- input 1))
           snd-r       (sound-in input)
           snd         (balance2 snd-l snd-r pan amp)]
       (out out_bus snd)))

   (defsynth sonic-pi-sound_in [amp 1
                                amp_slide 0
                                amp_slide_shape 1
                                amp_slide_curve 0
                                pan 0
                                pan_slide 0
                                pan_slide_shape 1
                                pan_slide_curve 0
                                attack 0
                                decay 0
                                sustain 1
                                release 0
                                attack_level 1
                                decay_level -1
                                sustain_level 1
                                env_curve 1

                                input 1
                                out_bus 0]
     (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           snd         (sound-in (- input 1))
           env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* env snd) pan amp))))

   (defsynth sonic-pi-sound_in_stereo [amp 1
                                       amp_slide 0
                                       amp_slide_shape 1
                                       amp_slide_curve 0
                                       pan 0
                                       pan_slide 0
                                       pan_slide_shape 1
                                       pan_slide_curve 0
                                       attack 0
                                       decay 0
                                       sustain 1
                                       release 0
                                       attack_level 1
                                       decay_level -1
                                       sustain_level 1
                                       env_curve 1

                                       input 1
                                       out_bus 0]
     (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           snd-l       (sound-in (- input 1))
           snd-r       (sound-in input)
           env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
           snd-l       (* env snd-l)
           snd-r       (* env snd-r)
           snd         (balance2 snd-l snd-r pan amp)]

       (out out_bus snd)))

   (defsynth sonic-pi-scope [bus 0
                             scope_num 0
                             max_frames 4096]
     (scope-out2 (in:ar bus 2) scope_num max_frames))

   (defsynth sonic-pi-server-info
    [response-id -1]
    (send-reply (impulse 2)
                "/sonic-pi/server-info"
                [(sample-rate)
                 (sample-dur)
                 (radians-per-sample)
                 (control-rate)
                 (control-dur)
                 (subsample-offset)
                 (num-output-buses)
                 (num-input-buses)
                 (num-audio-buses)
                 (num-control-buses)
                 (num-buffers)
                 (num-running-synths)]
                response-id))
   )


  (comment
    (core/save-synthdef sonic-pi-live_audio_mono)
    (core/save-synthdef sonic-pi-live_audio_stereo)
    (core/save-synthdef sonic-pi-sound_in)
    (core/save-synthdef sonic-pi-sound_in_stereo)
    (core/save-synthdef sonic-pi-mixer)
    (core/save-synthdef sonic-pi-basic_mixer)
    (core/save-synthdef sonic-pi-recorder)
    (core/save-synthdef sonic-pi-scope)
    (core/save-synthdef sonic-pi-server-info))

)
