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


(ns sonic-pi.samplers
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef

 (defsynth sonic-pi-basic_mono_player
   [buf [0 :ir]
    amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    attack [0.0 :ir]
    decay [0 :ir]
    sustain [-1 :ir]
    release [0.0 :ir]
    attack_level [1 :ir]
    decay_level [-1 :ir]
    sustain_level [1 :ir]
    env_curve [1 :ir]
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    lpf -1
    lpf_slide 0
    lpf_slide_shape 1
    lpf_slide_curve 0
    hpf -1
    hpf_slide 0
    hpf_slide_shape 1
    hpf_slide_curve 0
    rate [1 :ir]
    out_bus [0 :ir]]

   (let [amp            (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan            (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         lpf            (varlag lpf lpf_slide lpf_slide_curve lpf_slide_shape)
         hpf            (varlag hpf hpf_slide hpf_slide_curve hpf_slide_shape)
         scaled-rate    (* rate (buf-rate-scale:ir buf))
         lpf-freq       (midicps lpf)
         hpf-freq       (midicps hpf)
         use-lpf-filter (> lpf -1)
         use-hpf-filter (> hpf -1)
         dur            (* (/ 1 (abs rate)) (buf-dur:ir buf))
         start          (select:kr (< rate 0) [0
                                               (- (buf-frames:ir buf) 1)])
         sustain        (select:kr (= -1 sustain) [sustain (- dur attack release decay)])
         decay_level    (select:kr (= -1 decay_level) [decay_level sustain_level])
         env-dur        (+ attack sustain decay release)
         env            (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve))
         snd            (play-buf 1 buf scaled-rate 0 start)

         snd            (select use-lpf-filter [snd (overtone.live/lpf snd lpf-freq)])
         snd            (select use-hpf-filter [snd (overtone.live/hpf snd hpf-freq)])


         snd            (* env snd)]
     (line:kr 1 1 (+ 0.03 (min dur env-dur)) FREE)
     (out out_bus (pan2 snd pan amp))))

 (defsynth sonic-pi-basic_stereo_player
   [buf [0 :ir]
    amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    attack [0.0 :ir]
    decay [0 :ir]
    sustain [-1 :ir]
    release [0.0 :ir]
    attack_level [1 :ir]
    decay_level [-1 :ir]
    sustain_level [1 :ir]
    env_curve [1 :ir]
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    lpf -1
    lpf_slide 0
    lpf_slide_shape 1
    lpf_slide_curve 0
    hpf -1
    hpf_slide 0
    hpf_slide_shape 1
    hpf_slide_curve 0
    rate [1 :ir]
    out_bus [0 :ir]]

   (let [amp            (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan            (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         lpf            (varlag lpf lpf_slide lpf_slide_curve lpf_slide_shape)
         hpf            (varlag hpf hpf_slide hpf_slide_curve hpf_slide_shape)
         scaled-rate    (* rate (buf-rate-scale:ir buf))
         lpf-freq       (midicps lpf)
         hpf-freq       (midicps hpf)
         use-lpf-filter (> lpf -1)
         use-hpf-filter (> hpf -1)
         dur            (* (/ 1 (abs rate)) (buf-dur:ir buf))
         sustain        (select:kr (= -1 sustain) [sustain (- dur attack release decay)])
         decay_level    (select:kr (= -1 decay_level) [decay_level sustain_level])
         env-dur        (+ attack sustain decay release)
         env            (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve))
         start          (select:kr (< rate 0) [0
                                               (- (buf-frames:ir buf) 1)])
         [snd-l snd-r]  (play-buf 2 buf scaled-rate 0 start)
         snd-l          (select use-lpf-filter [snd-l (overtone.live/lpf snd-l lpf-freq)])
         snd-r          (select use-lpf-filter [snd-r (overtone.live/lpf snd-r lpf-freq)])

         snd-l          (select use-hpf-filter [snd-l (overtone.live/hpf snd-l hpf-freq)])
         snd-r          (select use-hpf-filter [snd-r (overtone.live/hpf snd-r hpf-freq)])

         snd-l          (* env snd-l)
         snd-r          (* env snd-r)
         snd            (balance2 snd-l snd-r pan amp)]
     (line:kr 1 1 (+ 0.03 (min dur env-dur)) FREE)
     (out out_bus snd)))


(defsynth sonic-pi-mono_player
   "Plays a mono buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf [0 :ir]
    amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0

    attack [0.0 :ir]
    decay [0 :ir]
    sustain [-1 :ir]
    release [0.0 :ir]
    attack_level [1 :ir]
    decay_level [-1 :ir]
    sustain_level [1 :ir]
    env_curve 1
    rate 1
    start 0
    finish 1

    lpf -1
    lpf_slide 0
    lpf_slide_shape 1
    lpf_slide_curve 0

    lpf_attack 0
    lpf_sustain -1
    lpf_decay 0
    lpf_release 0

    lpf_min -1
    lpf_min_slide 0
    lpf_min_slide_shape 1
    lpf_min_slide_curve 0

    lpf_init_level [-1 :ir]
    lpf_attack_level [-1 :ir]
    lpf_decay_level [-1 :ir]
    lpf_sustain_level [-1 :ir]
    lpf_release_level [-1 :ir]
    lpf_env_curve 1

    hpf -1
    hpf_slide 0
    hpf_slide_shape 1
    hpf_slide_curve 0

    hpf_max -1
    hpf_max_slide 0
    hpf_max_slide_shape 1
    hpf_max_slide_curve 0

    hpf_attack 0
    hpf_sustain -1
    hpf_decay 0
    hpf_release 0
    hpf_init_level [-1 :ir]
    hpf_attack_level [-1 :ir]
    hpf_decay_level [-1 :ir]
    hpf_sustain_level [-1 :ir]
    hpf_release_level [-1 :ir]
    hpf_env_curve 1

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
    compress 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 1
    pre_amp_slide_curve 0
    threshold 0.2
    threshold_slide 0
    threshold_slide_shape 1
    threshold_slide_curve 0
    clamp_time 0.01
    clamp_time_slide 0
    clamp_time_slide_shape 1
    clamp_time_slide_curve 0
    slope_above 0.5
    slope_above_slide 0
    slope_above_slide_shape 1
    slope_above_slide_curve 0
    slope_below 1
    slope_below_slide 0
    slope_below_slide_shape 1
    slope_below_slide_curve 0
    relax_time 0.01
    relax_time_slide 0
    relax_time_slide_shape 1
    relax_time_slide_curve 0
    out_bus 0]
  (let [decay_level            (select:kr (= -1 decay_level) [decay_level sustain_level])

        amp                    (varlag amp amp_slide amp_slide_curve amp_slide_shape)
        pan                    (varlag pan pan_slide pan_slide_curve pan_slide_shape)
        lpf                    (varlag lpf lpf_slide lpf_slide_curve lpf_slide_shape)
        lpf_min                (varlag lpf_min lpf_min_slide lpf_min_slide_curve lpf_min_slide_shape)
        hpf                    (varlag hpf hpf_slide hpf_slide_curve hpf_slide_shape)
        hpf_max                (varlag hpf_max hpf_max_slide hpf_max_slide_curve hpf_max_slide_shape)
        pitch                  (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
        window_size            (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
        pitch_dis              (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
        time_dis               (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)

        pre_amp                (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
        threshold              (varlag threshold threshold_slide threshold_slide_curve threshold_slide_shape)
        clamp_time             (varlag clamp_time clamp_time_slide clamp_time_slide_curve clamp_time_slide_shape)

        slope_above            (varlag slope_above slope_above_slide slope_above_slide_curve slope_above_slide_shape)
        slope_below            (varlag slope_below slope_below_slide slope_below_slide_curve slope_below_slide_shape)
        relax_time             (varlag relax_time relax_time_slide relax_time_slide_curve relax_time_slide_shape)

        used_lpf               (not= -1 lpf)

        used_lpf_init_level    (not= -1 lpf_init_level)
        used_lpf_attack_level  (not= -1 lpf_attack_level)
        used_lpf_decay_level   (not= -1 lpf_decay_level)
        used_lpf_sustain_level (not= -1 lpf_sustain_level)
        used_lpf_release_level (not= -1 lpf_release_level)

        used_lpf_attack        (not= 0  lpf_attack)
        used_lpf_decay         (not= 0  lpf_decay)
        used_lpf_release       (not= 0  lpf_release)
        used_lpf_sustain       (not= -1 lpf_sustain)
        used_lpf_min           (not= -1 lpf_min)

        used_hpf               (not= -1 hpf)
        used_hpf_init_level    (not= -1 hpf_init_level)
        used_hpf_attack_level  (not= -1 hpf_attack_level)
        used_hpf_decay_level   (not= -1 hpf_decay_level)
        used_hpf_sustain_level (not= -1 hpf_sustain_level)
        used_hpf_release_level (not= -1 hpf_release_level)

        used_hpf_attack        (not= 0  hpf_attack)
        used_hpf_decay         (not= 0  hpf_decay)
        used_hpf_release       (not= 0  hpf_release)
        used_hpf_sustain       (not= -1 hpf_sustain)
        used_hpf_max           (not= -1 hpf_max)

        use-lpf-env            (or used_lpf_attack_level
                                   used_lpf_decay_level
                                   used_lpf_sustain_level
                                   used_lpf_attack
                                   used_lpf_decay
                                   used_lpf_release
                                   used_lpf_sustain
                                   used_lpf_min)

        use-hpf-env            (or used_hpf_init_level
                                   used_hpf_attack_level
                                   used_hpf_decay_level
                                   used_hpf_sustain_level
                                   used_hpf_release_level
                                   used_hpf_attack
                                   used_hpf_decay
                                   used_hpf_release
                                   used_hpf_sustain
                                   used_hpf_max)

        use-lpf                (or used_lpf
                                   use-lpf-env)


        use-hpf                (or used_hpf
                                   use-hpf-env)

        lpf                    (select:kr used_lpf [130 lpf])
        hpf                    (select:kr used_hpf [50 hpf])
        hpf_max                (select:kr used_hpf_max [200 hpf_max])
        lpf_min                (select:kr used_lpf_min [30 lpf_min])

        lpf_release_level      (select:kr used_lpf_release_level [lpf lpf_release_level])
        lpf_sustain_level      (select:kr used_lpf_sustain_level [lpf_release_level lpf_sustain_level])
        lpf_decay_level        (select:kr used_lpf_decay_level [lpf_sustain_level lpf_decay_level])
        lpf_attack_level       (select:kr used_lpf_attack_level [lpf_decay_level lpf_attack_level])
        lpf_init_level         (select:kr used_lpf_init_level [lpf_min lpf_init_level])

        hpf_release_level      (select:kr used_hpf_release_level [hpf hpf_release_level])
        hpf_sustain_level      (select:kr used_hpf_sustain_level [hpf_release_level hpf_sustain_level])
        hpf_decay_level        (select:kr used_hpf_decay_level [hpf_sustain_level hpf_decay_level])
        hpf_attack_level       (select:kr used_hpf_attack_level [hpf_decay_level hpf_attack_level])
        hpf_init_level         (select:kr used_hpf_init_level [130 hpf_init_level])

        lpf_attack             (select:kr used_lpf_attack [attack lpf_attack])
        lpf_decay              (select:kr used_lpf_decay [decay lpf_decay])
        lpf_sustain            (select:kr used_lpf_sustain [sustain lpf_sustain])
        lpf_release            (select:kr used_lpf_release [release lpf_release])

        hpf_attack             (select:kr used_hpf_attack [attack hpf_attack])
        hpf_decay              (select:kr used_hpf_decay [decay hpf_decay])
        hpf_sustain            (select:kr used_hpf_sustain [sustain hpf_sustain])
        hpf_release            (select:kr used_hpf_release [release hpf_release])

        pitch_ratio            (midiratio pitch)
        lpf-freq               (midicps lpf)
        hpf-freq               (midicps hpf)
        hpf_max                (midicps hpf_max)
        lpf_min                (midicps lpf_min)

        n-frames               (- (buf-frames buf) 1)
        start-pos              (* start n-frames)
        end-pos                (* finish n-frames)
        n-start-pos            (select:kr (not-pos? rate) [start-pos end-pos])
        n-end-pos              (select:kr (not-pos? rate) [end-pos start-pos])
        rate                   (abs rate)
        play-time              (/ (* (buf-dur buf) (absdif finish start))
                                  rate)
        phase                  (line:ar :start n-start-pos :end n-end-pos :dur play-time)
        sustain                (select:kr (= -1 sustain) [sustain (- play-time attack release decay)])
        lpf_sustain            (select:kr (= -1 lpf_sustain) [lpf_sustain (- play-time lpf_attack lpf_release lpf_decay)])
        hpf_sustain            (select:kr (= -1 hpf_sustain) [hpf_sustain (- play-time hpf_attack hpf_release hpf_decay)])
        env-dur                (+ attack sustain decay release)
        env                    (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve))
        lpf-env                (midicps (env-gen (core/shaped-adsr lpf_attack, lpf_decay lpf_sustain (+ 0.1 lpf_release) lpf_init_level lpf_attack_level lpf_decay_level lpf_sustain_level lpf_release_level lpf_env_curve)))
        hpf-env                (midicps (env-gen (core/shaped-adsr hpf_attack, hpf_decay hpf_sustain (+ 0.1 hpf_release) hpf_init_level hpf_attack_level hpf_decay_level hpf_sustain_level hpf_release_level hpf_env_curve)))

        lpf-env                (select use-lpf-env [lpf-freq (min lpf-freq
                                                                  (max lpf-env lpf_min))])
        hpf-env                (select use-hpf-env [hpf-freq (max hpf-freq
                                                                  (min hpf-env hpf_max))])


        snd                    (* pre_amp (buf-rd 1 buf phase))

        snd                    (select:ar (not= 0 pitch)
                                          [snd
                                           (pitch-shift snd window_size pitch_ratio pitch_dis time_dis)])

        lpf-env                (select use-lpf-env [lpf-freq (min lpf-env lpf-freq)])
        hpf-env                (select use-hpf-env [hpf-freq (max hpf-freq



                                                                  (min hpf-env hpf_max))])




        snd                    (select use-lpf
                                       [snd
                                        (overtone.live/lpf snd lpf-env)])

        snd                    (select use-hpf
                                       [snd
                                        (overtone.live/hpf snd hpf-env)])

        snd                    (select norm [snd (normalizer snd)])
        snd                    (* env snd)

        compressed             (compander snd snd threshold
                                          slope_below slope_above
                                          clamp_time relax_time)

        snd                    (select:ar compress [snd compressed])


        snd                    (pan2 snd pan amp)]
     (line:kr 1 1 (+ 0.03 (min play-time env-dur)) FREE)
     (out out_bus snd)))

 (defsynth sonic-pi-stereo_player
   "Plays a mono buffer from start pos to finish pos (represented as
     values between 0 and 1). Outputs a stereo signal."
   [buf [0 :ir]
    amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0

    attack [0.0 :ir]
    decay [0 :ir]
    sustain [-1 :ir]
    release [0.0 :ir]
    attack_level [1 :ir]
    decay_level [-1 :ir]
    sustain_level [1 :ir]
    env_curve 1
    rate 1
    start 0
    finish 1

    lpf -1
    lpf_slide 0
    lpf_slide_shape 1
    lpf_slide_curve 0

    lpf_attack 0
    lpf_sustain -1
    lpf_decay 0
    lpf_release 0

    lpf_min -1
    lpf_min_slide 0
    lpf_min_slide_shape 1
    lpf_min_slide_curve 0

    lpf_init_level [-1 :ir]
    lpf_attack_level [-1 :ir]
    lpf_decay_level [-1 :ir]
    lpf_sustain_level [-1 :ir]
    lpf_release_level [-1 :ir]
    lpf_env_curve 1

    hpf -1
    hpf_slide 0
    hpf_slide_shape 1
    hpf_slide_curve 0

    hpf_max -1
    hpf_max_slide 0
    hpf_max_slide_shape 1
    hpf_max_slide_curve 0

    hpf_attack 0
    hpf_sustain -1
    hpf_decay 0
    hpf_release 0
    hpf_init_level [-1 :ir]
    hpf_attack_level [-1 :ir]
    hpf_decay_level [-1 :ir]
    hpf_sustain_level [-1 :ir]
    hpf_release_level [-1 :ir]
    hpf_env_curve 1

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
    compress 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 1
    pre_amp_slide_curve 0
    threshold 0.2
    threshold_slide 0
    threshold_slide_shape 1
    threshold_slide_curve 0
    clamp_time 0.01
    clamp_time_slide 0
    clamp_time_slide_shape 1
    clamp_time_slide_curve 0
    slope_above 0.5
    slope_above_slide 0
    slope_above_slide_shape 1
    slope_above_slide_curve 0
    slope_below 1
    slope_below_slide 0
    slope_below_slide_shape 1
    slope_below_slide_curve 0
    relax_time 0.01
    relax_time_slide 0
    relax_time_slide_shape 1
    relax_time_slide_curve 0
    out_bus 0]
   (let [decay_level            (select:kr (= -1 decay_level) [decay_level sustain_level])

         amp                    (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan                    (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         lpf                    (varlag lpf lpf_slide lpf_slide_curve lpf_slide_shape)
         lpf_min                (varlag lpf_min lpf_min_slide lpf_min_slide_curve lpf_min_slide_shape)
         hpf                    (varlag hpf hpf_slide hpf_slide_curve hpf_slide_shape)
         hpf_max                (varlag hpf_max hpf_max_slide hpf_max_slide_curve hpf_max_slide_shape)
         pitch                  (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
         window_size            (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
         pitch_dis              (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
         time_dis               (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)

         pre_amp                (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         threshold              (varlag threshold threshold_slide threshold_slide_curve threshold_slide_shape)
         clamp_time             (varlag clamp_time clamp_time_slide clamp_time_slide_curve clamp_time_slide_shape)

         slope_above            (varlag slope_above slope_above_slide slope_above_slide_curve slope_above_slide_shape)
         slope_below            (varlag slope_below slope_below_slide slope_below_slide_curve slope_below_slide_shape)
         relax_time             (varlag relax_time relax_time_slide relax_time_slide_curve relax_time_slide_shape)

         used_lpf               (not= -1 lpf)

         used_lpf_init_level    (not= -1 lpf_init_level)
         used_lpf_attack_level  (not= -1 lpf_attack_level)
         used_lpf_decay_level   (not= -1 lpf_decay_level)
         used_lpf_sustain_level (not= -1 lpf_sustain_level)
         used_lpf_release_level (not= -1 lpf_release_level)

         used_lpf_attack        (not= 0  lpf_attack)
         used_lpf_decay         (not= 0  lpf_decay)
         used_lpf_release       (not= 0  lpf_release)
         used_lpf_sustain       (not= -1 lpf_sustain)
         used_lpf_min           (not= -1 lpf_min)

         used_hpf               (not= -1 hpf)
         used_hpf_init_level    (not= -1 hpf_init_level)
         used_hpf_attack_level  (not= -1 hpf_attack_level)
         used_hpf_decay_level   (not= -1 hpf_decay_level)
         used_hpf_sustain_level (not= -1 hpf_sustain_level)
         used_hpf_release_level (not= -1 hpf_release_level)

         used_hpf_attack        (not= 0  hpf_attack)
         used_hpf_decay         (not= 0  hpf_decay)
         used_hpf_release       (not= 0  hpf_release)
         used_hpf_sustain       (not= -1 hpf_sustain)
         used_hpf_max           (not= -1 hpf_max)

         use-lpf-env            (or used_lpf_attack_level
                                    used_lpf_decay_level
                                    used_lpf_sustain_level
                                    used_lpf_attack
                                    used_lpf_decay
                                    used_lpf_release
                                    used_lpf_sustain
                                    used_lpf_min)

         use-hpf-env            (or used_hpf_init_level
                                    used_hpf_attack_level
                                    used_hpf_decay_level
                                    used_hpf_sustain_level
                                    used_hpf_release_level
                                    used_hpf_attack
                                    used_hpf_decay
                                    used_hpf_release
                                    used_hpf_sustain
                                    used_hpf_max)

         use-lpf                (or used_lpf
                                    use-lpf-env)


         use-hpf                (or used_hpf
                                    use-hpf-env)

         lpf                    (select:kr used_lpf [130 lpf])
         hpf                    (select:kr used_hpf [50 hpf])
         hpf_max                (select:kr used_hpf_max [200 hpf_max])
         lpf_min                (select:kr used_lpf_min [30 lpf_min])

         lpf_release_level      (select:kr used_lpf_release_level [lpf lpf_release_level])
         lpf_sustain_level      (select:kr used_lpf_sustain_level [lpf_release_level lpf_sustain_level])
         lpf_decay_level        (select:kr used_lpf_decay_level [lpf_sustain_level lpf_decay_level])
         lpf_attack_level       (select:kr used_lpf_attack_level [lpf_decay_level lpf_attack_level])
         lpf_init_level         (select:kr used_lpf_init_level [lpf_min lpf_init_level])

         hpf_release_level      (select:kr used_hpf_release_level [hpf hpf_release_level])
         hpf_sustain_level      (select:kr used_hpf_sustain_level [hpf_release_level hpf_sustain_level])
         hpf_decay_level        (select:kr used_hpf_decay_level [hpf_sustain_level hpf_decay_level])
         hpf_attack_level       (select:kr used_hpf_attack_level [hpf_decay_level hpf_attack_level])
         hpf_init_level         (select:kr used_hpf_init_level [130 hpf_init_level])

         lpf_attack             (select:kr used_lpf_attack [attack lpf_attack])
         lpf_decay              (select:kr used_lpf_decay [decay lpf_decay])
         lpf_sustain            (select:kr used_lpf_sustain [sustain lpf_sustain])
         lpf_release            (select:kr used_lpf_release [release lpf_release])

         hpf_attack             (select:kr used_hpf_attack [attack hpf_attack])
         hpf_decay              (select:kr used_hpf_decay [decay hpf_decay])
         hpf_sustain            (select:kr used_hpf_sustain [sustain hpf_sustain])
         hpf_release            (select:kr used_hpf_release [release hpf_release])


         pitch_ratio            (midiratio pitch)
         lpf-freq               (midicps lpf)
         hpf-freq               (midicps hpf)
         hpf_max                (midicps hpf_max)
         lpf_min                (midicps lpf_min)

         n-frames               (- (buf-frames buf) 1)
         start-pos              (* start n-frames)
         end-pos                (* finish n-frames)
         n-start-pos            (select:kr (not-pos? rate) [start-pos end-pos])
         n-end-pos              (select:kr (not-pos? rate) [end-pos start-pos])
         rate                   (abs rate)
         play-time              (/ (* (buf-dur buf) (absdif finish start))
                                   rate)
         phase                  (line:ar :start n-start-pos :end n-end-pos :dur play-time)
         sustain                (select:kr (= -1 sustain) [sustain (- play-time attack release decay)])
         lpf_sustain            (select:kr (= -1 lpf_sustain) [lpf_sustain (- play-time lpf_attack lpf_release lpf_decay)])
         hpf_sustain            (select:kr (= -1 hpf_sustain) [hpf_sustain (- play-time hpf_attack hpf_release hpf_decay)])
         env-dur                (+ attack sustain decay release)
         env                    (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve))
         lpf-env                (midicps (env-gen (core/shaped-adsr lpf_attack, lpf_decay lpf_sustain (+ 0.1 lpf_release) lpf_init_level lpf_attack_level lpf_decay_level lpf_sustain_level lpf_release_level lpf_env_curve)))
         hpf-env                (midicps (env-gen (core/shaped-adsr hpf_attack, hpf_decay hpf_sustain (+ 0.1 hpf_release) hpf_init_level hpf_attack_level hpf_decay_level hpf_sustain_level hpf_release_level hpf_env_curve)))

         lpf-env                (select use-lpf-env [lpf-freq (min lpf-freq
                                                                   (max lpf-env lpf_min))])
         hpf-env                (select use-hpf-env [hpf-freq (max hpf-freq
                                                                   (min hpf-env hpf_max))])

         [snd-l snd-r]          (* pre_amp (buf-rd 2 buf phase))

         snd-l                  (select:ar (not= 0 pitch)
                                           [snd-l
                                            (pitch-shift snd-l window_size pitch_ratio pitch_dis time_dis)])

         snd-r                  (select:ar (not= 0 pitch)
                                           [snd-r
                                            (pitch-shift snd-r window_size pitch_ratio pitch_dis time_dis)])


         snd-l                  (select use-lpf [snd-l (overtone.live/lpf snd-l lpf-env)])
         snd-r                  (select use-lpf [snd-r (overtone.live/lpf snd-r lpf-env)])

         snd-l                  (select use-hpf [snd-l (overtone.live/hpf snd-l hpf-env)])
         snd-r                  (select use-hpf [snd-r (overtone.live/hpf snd-r hpf-env)])

         snd-l                  (select norm [snd-l (normalizer snd-l)])
         snd-r                  (select norm [snd-r (normalizer snd-r)])
         snd-l                  (* env snd-l)
         snd-r                  (* env snd-r)

         control-sig            (/ (+ snd-l snd-r) 2)

         compressed-l           (compander snd-l control-sig threshold
                                           slope_below slope_above
                                           clamp_time relax_time)

         compressed-r           (compander snd-r control-sig threshold
                                           slope_below slope_above
                                           clamp_time relax_time)


         snd-l                  (select:ar compress [snd-l compressed-l])
         snd-r                  (select:ar compress [snd-r compressed-r])

         snd                    (balance2 snd-l snd-r pan amp)]

     (line:kr 1 1 (+ 0.03 (min play-time env-dur)) FREE)
     (out out_bus snd)))

;(show-graphviz-synth  sonic-pi-stereo_player)
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
    amp_slide_shape 1
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level -1
    sustain_level 1
    env_curve 1
    rate 1
    rate_slide 0
    rate_slide_shape 1
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
   (let [decay_level   (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
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
         env           (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

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
    amp_slide_shape 1
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    attack 0.0
    decay 0
    sustain -1
    release 0.0
    attack_level 1
    decay_level -1
    sustain_level 1
    env_curve 1
    rate 1
    rate_slide 0
    rate_slide_shape 1
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
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
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
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

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
