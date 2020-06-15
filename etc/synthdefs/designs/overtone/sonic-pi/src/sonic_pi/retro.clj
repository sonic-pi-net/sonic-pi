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

(ns sonic-pi.retro
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef


(defsynth sonic-pi-tb303
   "A simple clone of the sound of a Roland TB-303 bass synthesizer."
   [note     52                        ; midi note value input
    note_slide 0
    note_slide_shape 1
    note_slide_curve 0
    amp      1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    pan      0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    attack   0.01
    sustain  0
    decay 0
    release  1
    attack_level 1
    decay_level -1
    sustain_level 1
    env_curve 2
    cutoff   120
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    cutoff_attack -1
    cutoff_sustain -1
    cutoff_decay -1
    cutoff_release -1
    cutoff_min 30
    cutoff_min_slide 0
    cutoff_min_slide_shape 1
    cutoff_min_slide_curve 0
    cutoff_attack_level 1
    cutoff_decay_level -1
    cutoff_sustain_level 1
    cutoff_env_curve 2
    res      0.9                       ; rlpf resonance
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    wave     0                         ; 0=saw, 1=pulse, 2=tri
    pulse_width 0.5                    ; only for pulse wave
    pulse_width_slide 0
    pulse_width_slide_shape 1
    pulse_width_slide_curve 0
    out_bus  0]
   (let [decay_level        (select:kr (= -1 decay_level) [decay_level sustain_level])
         cutoff_decay_level (select:kr (= -1 cutoff_decay_level) [cutoff_decay_level cutoff_sustain_level])
         cutoff_attack      (select:kr (= -1 cutoff_attack) [cutoff_attack attack])
         cutoff_decay       (select:kr (= -1 cutoff_decay) [cutoff_decay decay])
         cutoff_sustain     (select:kr (= -1 cutoff_sustain) [cutoff_sustain sustain])
         cutoff_release     (select:kr (= -1 cutoff_release) [cutoff_release release])
         note               (varlag note note_slide note_slide_curve note_slide_shape)
         amp                (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge          1
         pan                (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff             (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff_min         (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         res                (lin-lin res 1 0 0 1)
         res                (varlag res res_slide res_slide_curve res_slide_shape)
         pulse_width        (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         freq               (midicps note)
         env                (env-gen (env-adsr-ng attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         filt-env           (env-gen (env-adsr-ng cutoff_attack cutoff_decay cutoff_sustain cutoff_release cutoff_attack_level cutoff_decay_level cutoff_sustain_level cutoff_env_curve))

         snd                (rlpf (select:ar wave [(saw freq)
                                                   (pulse freq pulse_width)
                                                   (* 2 (lf-tri freq))])

                                  (+ (midicps cutoff_min) (* filt-env (midicps cutoff) ))
                                  res)

         snd                (* amp-fudge env snd)]
     (out out_bus (pan2 snd pan amp))))


  (defsynth sonic-pi-hoover
    "Implementation of the rave hoover synth taken from Daniel Turczanski's Overtone implementation,
     which in turn is based on Dan Stowell's version here http://www.mcld.co.uk/blog/blog.php?254
     as described here http://jvmsoup.com/2012/11/28/hoover-sound-in-overtone/"
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
     attack 0.05
     decay 0.0
     sustain 0.0
     release 1
     attack_level 1
     decay_level -1
     sustain_level 1
     env_curve 1
     cutoff 130
     cutoff_slide 0
     cutoff_slide_shape 1
     cutoff_slide_curve 0
     res      0.1                       ; rlpf resonance
     res_slide 0
     res_slide_shape 1
     res_slide_curve 0
     pre_amp 10
     amp-fudge 2.5
     out_bus 0]
    (let [;; The pwm here is creating a special half-saw, half-square hybrid wave
          ;; that was apparently used on the Roland Juno that made this sound
          decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
          pwm         (lin-lin (sin-osc:kr (vec (repeatedly 3 #(ranged-rand 2 4)))) -1 1 0.125 0.875)
          note        (varlag note note_slide note_slide_curve note_slide_shape)
          amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
          cutoff-freq (midicps cutoff)
          res         (lin-lin res 1 0 0 1)
          res         (varlag res res_slide res_slide_curve res_slide_shape)
          freq        (midicps note)
          freq        (*
                       freq
                       (lin-exp (sin-osc:kr
                                 (vec (repeatedly 3 #(ranged-rand 2.9 3.1)))
                                 (vec (repeatedly 3 #(ranged-rand 0 (* 2 Math/PI))))
                                 ) -1 1 0.995 1.005))
          mix         (*
                       0.1
                       (apply +
                              (*
                               (lin-lin (lf-saw (* [0.25 0.5 1] freq) 1) -1 1 0 1)
                               (- 1 (lf-pulse:ar (* freq [0.5 1 2]) 0 pwm)))))
                                        ;bass
          mix         (+ mix (lf-par (* 0.25 freq) 0))
          mix         (mul-add mix 0.1 0)
                                        ;eq
          mix         (b-peak-eq mix 6000 1 3)
          mix         (b-peak-eq mix 3500 1 6)
                                        ;chorus
          mix         (+ mix
                         (* 0.5 (comb-c mix 1/200
                                        (lin-lin (sin-osc:kr 3 [(* 0.5 Math/PI) (* 1.5 Math/PI)]) -1 1 1/300 1/200)
                                        0)))
          env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
          output      (* pre_amp mix env)
          output      (rlpf output cutoff-freq res)]
      (out out_bus (pan2 output pan (* amp-fudge amp)))))


 (defsynth sonic-pi-supersaw [note 52
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
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.9
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         input       (lf-saw freq)
         shift1      (lf-saw 4)
         shift2      (lf-saw 7)
         shift3      (lf-saw 5)
         shift4      (lf-saw 2)
         comp1       (> input shift1)
         comp2       (> input shift2)
         comp3       (> input shift3)
         comp4       (> input shift4)
         output      (+ (- input comp1) (- input comp2) (- input comp3) (- input comp4))
         output      (- output input)
         output      (leak-dc:ar (* output 0.25))
         output      (normalizer (rlpf output cutoff-freq res))
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         output      (* amp-fudge env output)
         output      (pan2 output pan amp)]
     (out out_bus output)))

 (defsynth sonic-pi-zawa [note 52
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
                          cutoff 100
                          cutoff_slide 0
                          cutoff_slide_shape 1
                          cutoff_slide_curve 0
                          res 0.9
                          res_slide 0
                          res_slide_shape 1
                          res_slide_curve 0
                          phase 1
                          phase_slide 0
                          phase_slide_shape 1
                          phase_slide_curve 0
                          phase_offset 0
                          wave 3
                          disable_wave 0
                          invert_wave 0
                          pulse_width 0.5
                          pulse_width_slide 0
                          pulse_width_slide_shape 1
                          pulse_width_slide_curve 0
                          range 24
                          range_slide 0
                          range_slide_shape 1
                          range_slide_curve 0
                          out_bus 0]
   (let [decay_level         (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                (varlag note note_slide note_slide_curve note_slide_shape)
         amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge           0.5
         pan                 (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         range               (varlag range range_slide range_slide_curve range_slide_shape)
         wob_rate            (/ 1 phase)
         cutoff              (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res                 (lin-lin res 1 0 0 1)
         res                 (varlag res res_slide res_slide_curve res_slide_shape)
         freq                (midicps note)
         cutoff              (midicps cutoff)
         double_phase_offset (* 2 phase_offset)
         rate                (/ 1 phase)
         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
         ctl-wave            (* -1 ctl-wave ctl-wave-mul)

         saw-freq            (select:kr disable_wave [(midicps (lin-lin ctl-wave -1 1 note (+ note range)))
                                                      (midicps (+ note range))])
         snd                 (rlpf (sync-saw
                                    freq
                                    saw-freq)
                                   cutoff
                                   res)
         env                 (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         output              (* amp-fudge env snd)
         output              (pan2 output pan amp)]
     (out out_bus output)))


 (defsynth sonic-pi-prophet
   "Synth design adapted from:
    The Prophet Speaks (page 2)
    Steal This Sound,  Mitchell Sigman

   Dark and swirly, this synth uses Pulse Width Modulation (PWM) to
   create a timbre which continually moves around. This effect is
   created using the pulse ugen which produces a variable width square
   wave. We then control the width of the pulses using a variety of LFOs
   - sin-osc and lf-tri in this case. We use a number of these LFO
   modulated pulse ugens with varying LFO type and rate (and phase in
   some cases to provide the LFO with a different starting point. We
   then mix all these pulses together to create a thick sound and then
   feed it through a resonant low pass filter (rlpf).

   For extra bass, one of the pulses is an octave lower (half the
   frequency) and its LFO has a little bit of randomisation thrown into
   its frequency component for that extra bit of variety."

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
    attack 0.01
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
    out_bus 0 ]

   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.5
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (lin-lin res 1 0 0 1)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         snd         (mix [(pulse freq (* 0.1 (/ (+ 1.2 (sin-osc:kr 1)) )))
                           (pulse freq (* 0.8 (/ (+ 1.2 (sin-osc:kr 0.3) 0.7) 2)))
                           (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 )) 2)))
                           (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 0.19)) 2)))
                           (* 0.5 (pulse (/ freq 2) (* 0.8 (/ (+ 1.2 (lf-tri:kr (+ 2 (lf-noise2:kr 0.2))))
                                                              2))))])
         snd         (normalizer snd)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)

         snd         (rlpf (* env snd snd) cutoff-freq res)
         snd         (* amp-fudge env snd)
         snd         (leak-dc snd)
         snd         (pan2 snd pan amp)]

     (out out_bus snd)))

 (comment
   (core/save-synthdef sonic-pi-tb303)
   (core/save-synthdef sonic-pi-supersaw)
   (core/save-synthdef sonic-pi-hoover)
   (core/save-synthdef sonic-pi-prophet)
   (core/save-synthdef sonic-pi-zawa)



   ))
