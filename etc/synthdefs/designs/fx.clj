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

(ns sonic-pi.synths.fx
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))


(without-namespace-in-synthdef

 (defsynth sonic-pi-fx_krush
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    gain 4
    gain_slide 0
    gain_slide_shape 5
    gain_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix         (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp     (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         gain        (varlag gain gain_slide gain_slide_curve gain_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)

         [in-l in-r] (abs (* pre_amp (in in_bus 2)))
         new-l-sqr   (squared in-l)
         new-l       (/ (+ new-l-sqr (* gain in-l)) (+ new-l-sqr (* in-l (- gain 1)) 1))
         new-r-sqr   (squared in-r)
         new-r       (/ (+ new-r-sqr (* gain in-r)) (+ new-r-sqr (* in-r (- gain 1)) 1))
         new-l       (rlpf new-l cutoff-freq res)
         new-r       (rlpf new-r cutoff-freq res)
         fin-l       (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r       (x-fade2 in-r new-r (- (* mix 2) 1) amp)]

     (out out_bus [fin-l fin-r])))

 (defsynth sonic-pi-fx_bitcrusher
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    sample_rate 10000
    sample_rate_slide 0
    sample_rate_slide_shape 5
    sample_rate_slide_curve 0
    bits 8
    bits_slide 0
    bits_slide_shape 5
    bits_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         sample_rate   (varlag sample_rate sample_rate_slide sample_rate_slide_curve sample_rate_slide_shape)
         bits          (varlag bits bits_slide bits_slide_curve bits_slide_shape)
         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (decimator [in-l in-r] sample_rate bits)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



  (defsynth sonic-pi-fx_pitch_shift
     [amp 1
      amp_slide 0
      amp_slide_shape 5
      amp_slide_curve 0

      mix 1
      mix_slide 0
      mix_slide_shape 5
      mix_slide_curve 0

      pre_amp 1
      pre_amp_slide 0
      pre_amp_slide_shape 5
      pre_amp_slide_curve 0

      mod_amp 1
      mod_amp_slide 0
      mod_amp_slide_shape 5
      mod_amp_slide_curve 0
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
      in_bus 0
      out_bus 0]
     (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
           mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
           pitch         (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
           window_size   (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
           pitch_dis     (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
           time_dis      (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
           pitch_ratio   (midiratio pitch)

           [in-l in-r]   (* pre_amp (in in_bus 2))
           [new-l new-r] (pitch-shift [in-l in-r] window_size pitch_ratio pitch_dis time_dis)
           fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
           fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]


       (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_reverb
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 0.4
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    room 0.6
    room_slide 0
    room_slide_shape 5
    room_slide_curve 0
    damp 0.5
    damp_slide 0
    damp_slide_shape 5
    damp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix     (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         room    (varlag room room_slide room_slide_curve room_slide_shape)
         damp    (varlag damp damp_slide damp_slide_curve damp_slide_shape)
         [l r]   (* pre_amp (in:ar in_bus 2))
         snd     (* amp (free-verb2 l r mix room damp))]
     (out out_bus snd)))




 (defsynth sonic-pi-fx_level
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp (varlag amp amp_slide amp_slide_curve amp_slide_shape)]
     (out out_bus (* amp (in in_bus 2)))))




 (defsynth sonic-pi-fx_echo
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    phase 0.25
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    decay 8
    decay_slide 0
    decay_slide_shape 5
    decay_slide_curve 0
    max_phase 2
    amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         decay         (varlag decay decay_slide decay_slide_curve decay_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (+ [in-l in-r] (comb-n [in-l in-r] max_phase phase decay))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))




 (defsynth sonic-pi-fx_slicer
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    phase 0.25
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    amp_min 0
    amp_min_slide 0
    amp_min_slide_shape 5
    amp_min_slide_curve 0
    amp_max 1
    amp_max_slide 0
    amp_max_slide_shape 5
    amp_max_slide_curve 0
    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 5
    pulse_width_slide_curve 0
    smooth 0
    smooth_slide 0
    smooth_slide_shape 5
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 5
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 5
    smooth_down_slide_curve 0
    probability 0
    probability_slide 0
    probability_slide_shape 5
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 5
    prob_pos_slide_curve 0
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    seed 0
    rand_buf 0
    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         amp_min             (varlag amp_min amp_min_slide amp_min_slide_curve amp_min_slide_shape)
         amp_max             (varlag amp_max amp_max_slide amp_max_slide_curve amp_max_slide_shape)
         smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
         smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
         smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
         rate                (/ 1 phase)

         prob_pos            (clip prob_pos 0 1)
         probability         (clip probability 0 1)
         prob_pos            (lin-lin prob_pos 0 1 -1 1)
         probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
         prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
         use-prob            (> probability 0)

         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         double_phase_offset (* 2 phase_offset)



         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                      (impulse:kr rate))



         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
         ctl-wave            (* -1 ctl-wave-mul ctl-wave)
         ctl-wave            (select:kr use-prob [ctl-wave
                                                  (select:kr ctl-wave-prob [prob_pos ctl-wave])])
         slice-amp           (lin-lin ctl-wave -1 1 amp_min amp_max)

         slice-amp           (lag-ud slice-amp smooth_up smooth_down)
         slice-amp           (lag slice-amp smooth)
         [in-l in-r]         (* pre_amp (in in_bus 2))
         [new-l new-r]       (* slice-amp [in-l in-r])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_wobble
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    phase 0.5
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    cutoff_min 60
    cutoff_min_slide 0
    cutoff_min_slide_shape 5
    cutoff_min_slide_curve 0
    cutoff_max 120
    cutoff_max_slide 0
    cutoff_max_slide_shape 5
    cutoff_max_slide_curve 0
    res 0.2
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    pulse_width 0.5                     ; only for pulse wave
    pulse_width_slide 0                 ; only for pulse wave
    pulse_width_slide_shape 5           ; only for pulse wa_shape 5
    pulse_width_slide_curve 0           ; only for pulse wa_curve 0
    filter 0                            ;0=rlpf, 1=rhpf
    smooth 0
    smooth_slide 0
    smooth_slide_shape 5
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 5
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 5
    smooth_down_slide_curve 0
    phase_offset 0
    wave 0                              ;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0                       ;0=normal wave, 1=inverted wave
    probability 0
    probability_slide 0
    probability_slide_shape 5
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 5
    prob_pos_slide_curve 0
    seed 0
    rand_buf 0
    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         cutoff_min          (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         cutoff_max          (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
         smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
         smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
         smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
         rate                (/ 1 phase)

         prob_pos            (clip prob_pos 0 1)
         probability         (clip probability 0 1)
         prob_pos            (lin-lin prob_pos 0 1 -1 1)
         probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
         prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
         use-prob            (> probability 0)

         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         double_phase_offset (* 2 phase_offset)
         res                 (lin-lin res 1 0 0 1)
         res                 (varlag res res_slide res_slide_curve res_slide_shape)


         cutoff_min          (midicps cutoff_min)
         cutoff_max          (midicps cutoff_max)



         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                      (impulse:kr rate))

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
         ctl-wave            (* -1 ctl-wave-mul ctl-wave)
         ctl-wave            (select:kr use-prob [ctl-wave
                                                  (select:kr ctl-wave-prob [prob_pos ctl-wave])])

         cutoff-freq         (lin-exp:kr ctl-wave -1 1 cutoff_min cutoff_max)

         cutoff-freq         (lag-ud cutoff-freq smooth_up smooth_down)
         cutoff-freq         (lag cutoff-freq smooth)

         [in-l in-r]         (* pre_amp (in in_bus 2))

         new-l               (select:ar filter [(rlpf in-l cutoff-freq res)
                                                (rhpf in-l cutoff-freq res)])
         new-r               (select:ar filter [(rlpf in-r cutoff-freq res)
                                                (rhpf in-r cutoff-freq res)])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))

  (defsynth sonic-pi-fx_panslicer
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    phase 0.25
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    pan_min 0
    pan_min_slide 0
    pan_min_slide_shape 5
    pan_min_slide_curve 0
    pan_max 1
    pan_max_slide 0
    pan_max_slide_shape 5
    pan_max_slide_curve 0
    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 5
    pulse_width_slide_curve 0
    smooth 0
    smooth_slide 0
    smooth_slide_shape 5
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 5
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 5
    smooth_down_slide_curve 0
    probability 0
    probability_slide 0
    probability_slide_shape 5
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 5
    prob_pos_slide_curve 0
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    seed 0
    rand_buf 0
    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         pan_min             (varlag pan_min pan_min_slide pan_min_slide_curve pan_min_slide_shape)
         pan_max             (varlag pan_max pan_max_slide pan_max_slide_curve pan_max_slide_shape)
         smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
         smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
         smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
         rate                (/ 1 phase)
         pan_min             (clip pan_min -1 1)
         pan_max             (clip pan_max -1 1)
         prob_pos            (clip prob_pos 0 1)
         probability         (clip probability 0 1)
         prob_pos            (lin-lin prob_pos 0 1 -1 1)
         probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
         prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
         use-prob            (> probability 0)

         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         double_phase_offset (* 2 phase_offset)



         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                      (impulse:kr rate))



         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
         ctl-wave            (* -1 ctl-wave-mul ctl-wave)
         ctl-wave            (select:kr use-prob [ctl-wave
                                                  (select:kr ctl-wave-prob [prob_pos ctl-wave])])

         pan-val             ctl-wave

         pan-val             (lag-ud pan-val smooth_up smooth_down)
         pan-val             (lag pan-val smooth)
         [in-l in-r]         (* pre_amp (in in_bus 2))
         [new-l new-r]       (balance2 in-l in-r pan-val amp)
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_ixi_techno
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    phase 4
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    phase_offset 0
    cutoff_min 60
    cutoff_min_slide 0
    cutoff_min_slide_shape 5
    cutoff_min_slide_curve 0
    cutoff_max 120
    cutoff_max_slide 0
    cutoff_max_slide_shape 5
    cutoff_max_slide_curve 0
    res 0.2
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         rate          (/ 1 phase)
         cutoff_min    (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         cutoff_max    (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff_min    (midicps cutoff_min)
         cutoff_max    (midicps cutoff_max)
         freq          (lin-exp (sin-osc:kr rate (* (- phase_offset 0.25) (* Math/PI 2))) -1 1 cutoff_min cutoff_max)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rlpf [in-l in-r] freq res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_compressor
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    threshold 0.2
    threshold_slide 0
    threshold_slide_shape 5
    threshold_slide_curve 0
    clamp_time 0.01
    clamp_time_slide 0
    clamp_time_slide_shape 5
    clamp_time_slide_curve 0
    slope_above 0.5
    slope_above_slide 0
    slope_above_slide_shape 5
    slope_above_slide_curve 0
    slope_below 1
    slope_below_slide 0
    slope_below_slide_shape 5
    slope_below_slide_curve 0
    relax_time 0.01
    relax_time_slide 0
    relax_time_slide_shape 5
    relax_time_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         threshold     (varlag threshold threshold_slide threshold_slide_curve threshold_slide_shape)
         clamp_time    (varlag clamp_time clamp_time_slide clamp_time_slide_curve clamp_time_slide_shape)
         slope_above   (varlag slope_above slope_above_slide slope_above_slide_curve slope_above_slide_shape)
         slope_below   (varlag slope_below slope_below_slide slope_below_slide_curve slope_below_slide_shape)
         relax_time    (varlag relax_time relax_time_slide relax_time_slide_curve relax_time_slide_shape)

         src           (* pre_amp (in in_bus 2))
         [in-l in-r]   src

         control-sig   (/ (+ in-l in-r) 2)

         [new-l new-r] (compander src control-sig threshold
                                  slope_below slope_above
                                  clamp_time relax_time)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_rlpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rlpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_nrlpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (rlpf [in-l in-r] cutoff res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_rhpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 10
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rhpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_nrhpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 10
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (rhpf [in-l in-r] cutoff res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_hpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 10
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (hpf [in-l in-r] cutoff)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_nhpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 10
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (hpf [in-l in-r] cutoff))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_lpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (lpf [in-l in-r] cutoff)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_nlpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (lpf [in-l in-r] cutoff))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_normaliser
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    level 1
    level_slide 0
    level_slide_shape 5
    level_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         level         (varlag level level_slide level_slide_curve level_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer [in-l in-r] level)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_distortion
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    distort 0.5
    distort_slide 0
    distort_slide_shape 5
    distort_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         distort       (varlag distort distort_slide distort_slide_curve distort_slide_shape)
         k             (/ (* 2 distort) (- 1 distort))

         src           (* pre_amp (in in_bus 2))
         [new-l new-r] (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
         [in-l in-r]   src
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_pan
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (balance2 in-l in-r pan amp)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_bpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    centre 100
    centre_slide 0
    centre_slide_shape 5
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
         freq          (midicps centre)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (bpf [in-l in-r] freq res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 (defsynth sonic-pi-fx_rbpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    centre 100
    centre_slide 0
    centre_slide_shape 5
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
         freq          (midicps centre)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (resonz [in-l in-r] freq res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))

 (defsynth sonic-pi-fx_nrbpf
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0
    centre 100
    centre_slide 0
    centre_slide_shape 5
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
         freq          (midicps centre)
         res           (lin-lin res 1 0 0 1)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (resonz [in-l in-r] freq res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))

  (defsynth sonic-pi-fx_octaver
    [amp 1
     amp_slide 0
     amp_slide_shape 5
     amp_slide_curve 0
     mix 1
     mix_slide 0
     mix_slide_shape 5
     mix_slide_curve 0
     pre_amp 1
     pre_amp_slide 0
     pre_amp_slide_shape 5
     pre_amp_slide_curve 0
     amp 1
     amp_slide 0
     amp_slide_shape 5
     amp_slide_curve 0
     oct1_amp 1
     oct1_amp_slide 0
     oct1_amp_slide_shape 5
     oct1_amp_slide_curve 0
     oct2_amp 1
     oct2_amp_slide 0
     oct2_amp_slide_shape 5
     oct2_amp_slide_curve 0
     oct3_amp 1
     oct3_amp_slide 0
     oct3_amp_slide_shape 5
     oct3_amp_slide_curve 0
     in_bus 0
     out_bus 0]
    (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          oct1_amp      (varlag oct1_amp oct1_amp_slide oct1_amp_slide_curve oct1_amp_slide_shape)
          oct2_amp      (varlag oct2_amp oct2_amp_slide oct2_amp_slide_curve oct2_amp_slide_shape)
          oct3_amp      (varlag oct3_amp oct3_amp_slide oct3_amp_slide_curve oct3_amp_slide_shape)
          mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
          pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
          direct-lpf    (lpf (* pre_amp (in in_bus 2)) 440)
          super-oct     (abs direct-lpf)
          sub-oct       (toggle-ff:ar direct-lpf)
          sub-sub-oct   (toggle-ff:ar sub-oct)

          [in-l in-r]   (* pre_amp (in in_bus 2))
          [new-l new-r] (+ (* super-oct oct1_amp) (* direct-lpf sub-oct oct2_amp) (* direct-lpf sub-sub-oct oct3_amp))
          fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
          fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
      (out out_bus [fin-l fin-r])))

  (defsynth sonic-pi-fx_ring_mod
    [amp 1
     amp_slide 0
     amp_slide_shape 5
     amp_slide_curve 0
     mix 1
     mix_slide 0
     mix_slide_shape 5
     mix_slide_curve 0
     pre_amp 1
     pre_amp_slide 0
     pre_amp_slide_shape 5
     pre_amp_slide_curve 0
     freq 100
     freq_slide 0
     freq_slide_shape 5
     freq_slide_curve 0
     mod_amp 1
     mod_amp_slide 0
     mod_amp_slide_shape 5
     mod_amp_slide_curve 0
     in_bus 0
     out_bus 0]
    (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
          pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
          mod_amp       (varlag mod_amp mod_amp_slide mod_amp_slide_curve mod_amp_slide_shape)
          freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
          freq          (midicps freq)

          [in-l in-r]   (* pre_amp (in in_bus 2))
          [new-l new-r] (limiter (ring1 [in-l in-r] (* mod_amp (sin-osc freq))))
          fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
          fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
      (out out_bus [fin-l fin-r])
      ))

  (defsynth sonic-pi-fx_flanger
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0

    mix 1
    mix_slide 0
    mix_slide_shape 5
    mix_slide_curve 0

    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 5
    pre_amp_slide_curve 0

    phase 4
    phase_slide 0
    phase_slide_shape 5
    phase_slide_curve 0
    phase_offset 0
    wave 4
    invert_wave 0
    stereo_invert_wave 0

    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 5
    pulse_width_slide_curve 0

    delay 5
    delay_slide 0
    delay_slide_shape 5
    delay_slide_curve 0

    max_delay 20

    depth 5
    depth_slide 0
    depth_slide_shape 5
    depth_slide_curve 0

    feedback 0
    feedback_slide 0
    feedback_slide_shape 5
    feedback_slide_curve 0

    decay 2
    decay_slide 0
    decay_slide_shape 5
    decay_slide_curve 0

    invert_flange 0

    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         delay               (varlag delay delay_slide delay_slide_curve delay_slide_shape)
         depth               (varlag depth depth_slide depth_slide_curve depth_slide_shape)
         feedback            (varlag feedback feedback_slide feedback_slide_curve feedback_slide_shape)
         decay               (varlag decay decay_slide decay_slide_curve decay_slide_shape)
         rate                (/ 1 phase)
         [in-l in-r]         (* pre_amp (in in_bus 2))

         [local-l local-r]   (local-in:ar 2)

         double_phase_offset (* 2 phase_offset)
         rate                (/ 1 phase)
         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))
                                              (lf-cub:kr rate (+ phase_offset 0.5))
                                              ])

         ctl-wave            (/ (+ ctl-wave 1) 2)
         inverted-ctl-wave   (- 1 ctl-wave)

         ctl-wave-r          (select:kr invert_wave [ctl-wave
                                                     inverted-ctl-wave])

         ctl-wave-l          (select:kr stereo_invert_wave [ctl-wave-r
                                                            (- 1 ctl-wave-r)])

         delay-l             (allpass-c  (+ (limiter (* local-l feedback)) (* (- 1 feedback) in-l))
                                         (/ max_delay 1000)
                                         (max
                                          (mul-add ctl-wave-l (/ depth 1000) (/ delay 1000))
                                          0)
                                         (/ decay 1000))

         delay-r             (allpass-c  (+ (limiter (* local-r feedback)) (* (- 1 feedback) in-r))
                                         (/ max_delay 1000)
                                         (max
                                          (mul-add ctl-wave-r (/ depth 1000) (/ delay 1000))
                                          0)
                                         (/ decay 1000))

         flange-wave-mul     (+ (* -2 (> invert_flange 0)) 1)
         new-l               (+ in-l (* flange-wave-mul delay-l))
         new-r               (+ in-r (* flange-wave-mul delay-r))
         lout                (local-out:ar [new-l new-r])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 ;;(def ab (audio-bus 2))
 ;;(def g (group :after (foundation-default-group)))
 ;;(sonic-pi-fx_rbpf [:head g] :in_bus ab)

 ;;(run (out ab (pan2 (saw))))

 ;;(kill sonic-pi-fx_rbpf)

 (comment
   (core/save-synthdef sonic-pi-fx_krush)
   (core/save-synthdef sonic-pi-fx_bitcrusher)
   (core/save-synthdef sonic-pi-fx_reverb)
   (core/save-synthdef sonic-pi-fx_level)
   (core/save-synthdef sonic-pi-fx_echo)
   (core/save-synthdef sonic-pi-fx_slicer)
   (core/save-synthdef sonic-pi-fx_panslicer)
   (core/save-synthdef sonic-pi-fx_wobble)
   (core/save-synthdef sonic-pi-fx_ixi_techno)
   (core/save-synthdef sonic-pi-fx_compressor)
   (core/save-synthdef sonic-pi-fx_rlpf)
   (core/save-synthdef sonic-pi-fx_nrlpf)
   (core/save-synthdef sonic-pi-fx_rhpf)
   (core/save-synthdef sonic-pi-fx_nrhpf)
   (core/save-synthdef sonic-pi-fx_hpf)
   (core/save-synthdef sonic-pi-fx_nhpf)
   (core/save-synthdef sonic-pi-fx_lpf)
   (core/save-synthdef sonic-pi-fx_nlpf)
   (core/save-synthdef sonic-pi-fx_normaliser)
   (core/save-synthdef sonic-pi-fx_distortion)
   (core/save-synthdef sonic-pi-fx_pan)
   (core/save-synthdef sonic-pi-fx_bpf)
   (core/save-synthdef sonic-pi-fx_rbpf)
   (core/save-synthdef sonic-pi-fx_nrbpf)
   (core/save-synthdef sonic-pi-fx_pitch_shift)
   (core/save-synthdef sonic-pi-fx_ring_mod)
   (core/save-synthdef sonic-pi-fx_octaver)
   (core/save-synthdef sonic-pi-fx_flanger)
   ))
