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


(ns sonic-pi.synths.samplers
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

(without-namespace-in-synthdef

 (defsynth sonic-pi-basic_mono_player
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    rate 1
    out_bus 0]

   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         res         (lin-lin:kr res 1 0 0 1)
         rate        (* rate (buf-rate-scale buf))
         cutoff-freq (midicps cutoff)
         use-filter  (> cutoff 0)
         killer      (line:kr 0 1 (* (/ 1 (abs rate)) (buf-dur buf)) :action FREE)
         start       (select:kr (< rate 0) [0
                                            (- (buf-frames buf) 1)])
         snd         (play-buf 1 buf rate 0 start)
         snd         (select use-filter [snd (rlpf snd cutoff-freq res)])]

     (out out_bus (pan2 snd pan amp))))

 (defsynth sonic-pi-basic_stereo_player
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    rate 1
    out_bus 0]

   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         res           (lin-lin:kr res 1 0 0 1)
         rate          (* rate (buf-rate-scale buf))
         cutoff-freq   (midicps cutoff)
         use-filter    (> cutoff 0)
         killer        (line:kr 0 1 (* (/ 1 (abs rate)) (buf-dur buf)) :action FREE)
         start         (select:kr (< rate 0) [0
                                              (- (buf-frames buf) 1)])
         [snd-l snd-r] (play-buf 2 buf rate 0 start)
         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])
         snd           (balance2 snd-l snd-r pan amp)]

     (out out_bus snd)))


(defsynth sonic-pi-mono_player
   "Plays a mono buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level 1
    sustain_level 1
    env_curve 2
    env_curve 1
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    rate 1
    start 0
    finish 1
    norm 0
    pitch 0
    pitch_slide 0
    pitch_slide_shape 1
    pitch_slide_curve 0
    window_size 0.2
    window_size_slide 0
    window_size_slide_shape 1
    window_size_slide_curve 0
    pitch_dis 0.0
    pitch_dis_slide 0
    pitch_dis_slide_shape 1
    pitch_dis_slide_curve 0
    time_dis 0.0
    time_dis_slide 0
    time_dis_slide_shape 1
    time_dis_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         pitch       (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
         window_size (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
         pitch_dis   (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
         time_dis    (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
         pitch_ratio (midiratio pitch)
         cutoff-freq (midicps cutoff)
         use-filter  (> cutoff 0)
         n-frames    (- (buf-frames buf) 1)
         start-pos   (* start n-frames)
         end-pos     (* finish n-frames)
         n-start-pos (select:kr (not-pos? rate) [start-pos end-pos])
         n-end-pos   (select:kr (not-pos? rate) [end-pos start-pos])
         rate        (abs rate)
         play-time   (/ (* (buf-dur buf) (absdif finish start))
                        rate)
         phase       (line:ar :start n-start-pos :end n-end-pos :dur play-time :action FREE)
         sustain     (select:kr (= -1 sustain) [sustain (- play-time attack release decay)])
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (buf-rd 1 buf phase)
         snd         (select:ar (not= 0 pitch)
                                [snd
                                 (pitch-shift snd window_size pitch_ratio pitch_dis time_dis)])
         snd         (select use-filter [snd (rlpf snd cutoff-freq res)])
         snd         (select norm [snd (normalizer snd)])
         snd         (* env snd)
         snd         (pan2 snd pan amp)]
     (out out_bus snd)))

 (defsynth sonic-pi-stereo_player
   "Plays a mono buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level 1
    sustain_level 1
    env_curve 2
    rate 1
    start 0
    finish 1
    norm 0
    pitch 0
    pitch_slide 0
    pitch_slide_shape 1
    pitch_slide_curve 0
    window_size 0.2
    window_size_slide 0
    window_size_slide_shape 1
    window_size_slide_curve 0
    pitch_dis 0.0
    pitch_dis_slide 0
    pitch_dis_slide_shape 1
    pitch_dis_slide_curve 0
    time_dis 0.0
    time_dis_slide 0
    time_dis_slide_shape 1
    time_dis_slide_curve 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         pitch         (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
         window_size   (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
         pitch_dis     (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
         time_dis      (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
         pitch_ratio   (midiratio pitch)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq   (midicps cutoff)
         use-filter    (> cutoff 0)
         n-frames      (- (buf-frames buf) 1)
         start-pos     (* start n-frames)
         end-pos       (* finish n-frames)
         n-start-pos   (select:kr (not-pos? rate) [start-pos end-pos])
         n-end-pos     (select:kr (not-pos? rate) [end-pos start-pos])
         rate          (abs rate)
         play-time     (/ (* (buf-dur buf) (absdif finish start))
                          rate)
         phase         (line:ar :start n-start-pos :end n-end-pos :dur play-time :action FREE)
         sustain       (select:kr (= -1 sustain) [sustain (- play-time attack release decay)])
         env           (env-gen (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

         [snd-l snd-r] (buf-rd 2 buf phase)
         snd-l         (select:ar (not= 0 pitch)
                                  [snd-l
                                   (pitch-shift snd-l window_size pitch_ratio pitch_dis time_dis)])

         snd-r         (select:ar (not= 0 pitch)
                                  [snd-r
                                   (pitch-shift snd-r window_size pitch_ratio pitch_dis time_dis)])
         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])
         snd-l         (select norm [snd-l (normalizer snd-l)])
         snd-r         (select norm [snd-r (normalizer snd-r)])
         snd-l         (* env snd-l)
         snd-r         (* env snd-r)
         snd           (balance2 snd-l snd-r pan amp)]

      (out out_bus snd)))





 (comment
   (core/save-synthdef sonic-pi-mono_player)
   (core/save-synthdef sonic-pi-stereo_player)
   (core/save-synthdef sonic-pi-basic_mono_player)
   (core/save-synthdef sonic-pi-basic_stereo_player)))



;; these don't currently work on the Raspberry Pi
(comment
   (defsynth sonic-pi-stereo_player-future
   "Plays a stereo buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level 1
    sustain_level 1
    env_curve 2
    rate 1
    rate_slide 0
    rate_slide_shape 5
    rate_slide_curve 0
    start 0
    finish 1
    norm 0
    rate-shape 5
    rate-curvature 0
    pitch 0
    pitch_slide 0
    pitch_slide_shape 1
    pitch_slide_curve 0
    window_size 0.2
    window_size_slide 0
    window_size_slide_shape 1
    window_size_slide_curve 0
    pitch_dis 0.0
    pitch_dis_slide 0
    pitch_dis_slide_shape 1
    pitch_dis_slide_curve 0
    time_dis 0.0
    time_dis_slide 0
    time_dis_slide_shape 1
    time_dis_slide_curve 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         rate          (varlag rate rate_slide rate_slide_curve rate_slide_shape)
         pitch         (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
         window_size   (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
         pitch_dis     (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
         time_dis      (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
         pitch_ratio   (midiratio pitch)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq   (midicps cutoff)
         use-filter    (> cutoff 0)
         n-start       (select:kr (not-pos? rate) [start finish])
         n-finish      (select:kr (not-pos? rate) [finish start])
         rate          (abs rate)

         n-frames      (- (buf-frames buf) 1)

         length        (* (buf-dur buf) (absdif finish start))
         val           (local-in:ar 1)
         time          (/ (- length (* length val)) rate)
         gate          (+ (+ (impulse:ar 0 0) (> (abs (hpz1:ar rate)) 0)))
         val           (env-gen:ar [0 1 -99 -99 1 time rate-shape rate-curvature] gate)


         phase         (* (lin-lin val 0 1 n-start n-finish) n-frames)
         sustain       (select:kr (= -1 sustain) [sustain (- (/ length rate) attack release decay)])
         env           (env-gen (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

         [snd-l snd-r] (buf-rd 2 buf phase)


         snd-l         (select:ar (not= 0 pitch)
                                  [snd-l
                                   (pitch-shift snd-l window_size pitch_ratio pitch_dis time_dis)])

         snd-r         (select:ar (not= 0 pitch)
                                  [snd-r
                                   (pitch-shift snd-r window_size pitch_ratio pitch_dis time_dis)])

         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])

         snd-l         (select norm [snd-l (normalizer snd-l)])
         snd-r         (select norm [snd-r (normalizer snd-r)])
         snd-l         (* env snd-l)
         snd-r         (* env snd-r)
         snd           (balance2 snd-l snd-r pan amp)]

     (local-out:ar val)
     (out out_bus snd)))

     (defsynth sonic-pi-mono_player-future
   "Plays a mono buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf 0
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level 1
    sustain_level 1
    env_curve 2
    rate 1
    rate_slide 0
    rate_slide_shape 5
    rate_slide_curve 0
    start 0
    finish 1
    norm 0
    rate-shape 5
    rate-curvature 0
    pitch 0
    pitch_slide 0
    pitch_slide_shape 1
    pitch_slide_curve 0
    window_size 0.2
    window_size_slide 0
    window_size_slide_shape 1
    window_size_slide_curve 0
    pitch_dis 0.0
    pitch_dis_slide 0
    pitch_dis_slide_shape 1
    pitch_dis_slide_curve 0
    time_dis 0.0
    time_dis_slide 0
    time_dis_slide_shape 1
    time_dis_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         rate        (varlag rate rate_slide rate_slide_curve rate_slide_shape)
         pitch       (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
         window_size (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
         pitch_dis   (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
         time_dis    (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
         pitch_ratio (midiratio pitch)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         use-filter  (> cutoff 0)
         n-start     (select:kr (not-pos? rate) [start finish])
         n-finish    (select:kr (not-pos? rate) [finish start])
         rate        (abs rate)

         n-frames    (- (buf-frames buf) 1)

         length      (* (buf-dur buf) (absdif finish start))
         val         (local-in:ar 1)
         time        (/ (- length (* length val)) rate)
         gate        (+ (+ (impulse:ar 0 0) (> (abs (hpz1:ar rate)) 0)))
         val         (env-gen:ar [0 1 -99 -99 1 time rate-shape rate-curvature] gate)


         phase       (* (lin-lin val 0 1 n-start n-finish) n-frames)
         sustain     (select:kr (= -1 sustain) [sustain (- (/ length rate) attack release decay)])
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

         snd         (buf-rd 2 buf phase)




         snd         (select:ar (not= 0 pitch)
                                [snd
                                 (pitch-shift snd window_size pitch_ratio pitch_dis time_dis)])
         snd         (select use-filter [snd (rlpf snd cutoff-freq res)])
         snd         (select norm [snd (normalizer snd)])
         snd         (* env snd)

         snd         (pan2 snd pan amp)]

     (local-out:ar val)
     (out out_bus snd))))
