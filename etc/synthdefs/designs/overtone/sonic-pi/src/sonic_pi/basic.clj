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


(ns sonic-pi.basic
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

(without-namespace-in-synthdef
 (defsynth sonic-pi-beep [note 52
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
                          out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         freq        (midicps note)
         snd         (sin-osc freq)
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))

 (defsynth sonic-pi-pulse [note 52
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
                           pulse_width 0.5
                           pulse_width_slide 0
                           pulse_width_slide_shape 1
                           pulse_width_slide_curve 0
                           out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.8
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         pulse_width (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         snd         (pulse freq pulse_width)
         snd         (lpf snd cutoff-freq)
         snd         (normalizer snd)
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))






 (defsynth sonic-pi-subpulse [note 52
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
                              pulse_width 0.5
                              pulse_width_slide 0
                              pulse_width_slide_shape 1
                              pulse_width_slide_curve 0
                              sub_amp 1
                              sub_amp_slide 0
                              sub_amp_slide_shape 1
                              sub_amp_slide_curve 0
                              sub_detune -12
                              sub_detune_slide 0
                              sub_detune_slide_shape 1
                              sub_detune_slide_curve 0
                              out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.8
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         pulse_width (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         sub_detune  (varlag sub_detune sub_detune_slide sub_detune_slide_curve sub_detune_slide_shape)
         freq        (midicps note)
         beep-freq   (midicps (+ note sub_detune))
         cutoff-freq (midicps cutoff)
         snd         (+ (pulse freq pulse_width)
                        (* sub_amp (sin-osc beep-freq)))
         snd         (lpf snd cutoff-freq)
         snd         (normalizer snd)
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))



 (defsynth sonic-pi-square [note 52
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
                            out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.8
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         snd         (pulse freq 0.5)
         snd         (lpf snd cutoff-freq)
         snd         (normalizer snd)
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


 (defsynth sonic-pi-saw [note 52
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
                         out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.8
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         snd         (normalizer (lpf (saw freq) cutoff-freq))
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


 (defsynth sonic-pi-tri [note 52
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
                         out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.4
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         snd         (normalizer (lpf (lf-tri freq) cutoff-freq))
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge snd env) pan amp))))

 (defsynth sonic-pi-dsaw [note 52
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
                          detune 0.1
                          detune_slide 0
                          detune_slide_shape 1
                          detune_slide_curve 0
                          out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         detune      (varlag detune detune_slide detune_slide_curve detune_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         detune-freq (midicps (+ note detune))
         snd         (normalizer (lpf (mix (saw [freq detune-freq])) cutoff-freq))
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge snd env) pan amp))))


  (defsynth sonic-pi-dtri [note 52
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
                          detune 0.1
                          detune_slide 0
                          detune_slide_shape 1
                          detune_slide_curve 0
                          out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         detune      (varlag detune detune_slide detune_slide_curve detune_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq        (midicps note)
         cutoff-freq (midicps cutoff)
         detune-freq (midicps (+ note detune))
         snd         (normalizer (lpf (mix (lf-tri [freq detune-freq])) cutoff-freq))
         env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge snd env) pan amp))))


 (defsynth sonic-pi-dpulse [note 52
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
                            detune 0.1
                            detune_slide 0
                            detune_slide_shape 1
                            detune_slide_curve 0
                            pulse_width 0.5
                            pulse_width_slide 0
                            pulse_width_slide_shape 1
                            pulse_width_slide_curve 0

                            dpulse_width -1
                            dpulse_width_slide -1
                            dpulse_width_slide_shape -1
                            dpulse_width_slide_curve -1
                            out_bus 0]
   (let [decay_level              (select:kr (= -1 decay_level) [decay_level sustain_level])
         dpulse_width             (select:kr (= -1 dpulse_width) [dpulse_width pulse_width])
         dpulse_width_slide       (select:kr (= -1 dpulse_width_slide) [dpulse_width_slide pulse_width_slide])
         dpulse_width_slide_shape (select:kr (= -1 dpulse_width_slide_shape) [dpulse_width pulse_width_slide_shape])
         dpulse_width_slide_curve (select:kr (= -1 dpulse_width_slide_curve) [dpulse_width pulse_width_slide_curve])

         note                     (varlag note note_slide note_slide_curve note_slide_shape)
         amp                      (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge                1.1
         pan                      (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         detune                   (varlag detune detune_slide detune_slide_curve detune_slide_shape)
         cutoff                   (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         pulse_width              (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         dpulse_width             (varlag dpulse_width dpulse_width_slide dpulse_width_slide_curve dpulse_width_slide_shape)
         freq                     (midicps note)
         cutoff-freq              (midicps cutoff)
         detune-freq              (midicps (+ note detune))
         snd                      (normalizer (lpf (/ (+ (pulse freq pulse_width) (pulse detune-freq dpulse_width)) 2) cutoff-freq))
         env                      (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge snd env) pan amp))))



 (defsynth sonic-pi-fm [note 52
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
                        divisor 2.0
                        divisor_slide 0
                        divisor_slide_shape 1
                        divisor_slide_curve 0
                        depth 1.0
                        depth_slide 0
                        depth_slide_shape 1
                        depth_slide_curve 0

                        out_bus 0]
   (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
         note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.8
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         divisor     (varlag divisor divisor_slide divisor_slide_curve divisor_slide_shape)
         depth       (varlag depth depth_slide depth_slide_curve depth_slide_shape)
         carrier     (midicps note)
         modulator   (/ carrier divisor)
         cutoff-freq (midicps cutoff)
         env         (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd         (sin-osc (+ carrier
                                 (* env (* carrier depth) (sin-osc modulator))))
         snd         (lpf snd cutoff-freq)
         ]

     (out out_bus (pan2 (* amp-fudge (* env snd)) pan amp))))


 (defsynth sonic-pi-mod_fm [note 52
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
                            mod_phase 0.25
                            mod_phase_slide 0
                            mod_phase_slide_shape 1
                            mod_phase_slide_curve 0
                            mod_range 5
                            mod_range_slide 0
                            mod_range_slide_shape 1
                            mod_range_slide_curve 0
                            mod_pulse_width 0.5
                            mod_pulse_width_slide 0
                            mod_pulse_width_slide_shape 1
                            mod_pulse_width_slide_curve 0
                            mod_phase_offset 0
                            mod_wave 1
                            mod_invert_wave 0
                            divisor 2.0
                            divisor_slide 0
                            divisor_slide_shape 1
                            divisor_slide_curve 0
                            depth 1.0
                            depth_slide 0
                            depth_slide_shape 1
                            depth_slide_curve 0
                            out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide note_slide_curve note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               1
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)
         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide mod_range_slide_curve mod_range_slide_shape)
         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         note                    (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps note)
         cutoff-freq             (midicps cutoff)
         divisor                 (varlag divisor divisor_slide divisor_slide_curve divisor_slide_shape)
         depth                   (varlag depth depth_slide depth_slide_curve depth_slide_shape)
         carrier                 freq
         modulator               (/ carrier divisor)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)
         snd                     (sin-osc (+ carrier
                                             (* env (* carrier depth) (sin-osc modulator))))
         snd                     (lpf snd cutoff-freq)
         ]

     (out out_bus (pan2 (* amp-fudge (* env snd)) pan amp))))



 (defsynth sonic-pi-mod_saw [note 52
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
                             mod_phase 0.25
                             mod_phase_slide 0
                             mod_phase_slide_shape 1
                             mod_phase_slide_curve 0

                             mod_range 5
                             mod_range_slide 0
                             mod_range_slide_shape 1
                             mod_range_slide_curve 0
                             mod_pulse_width 0.5
                             mod_pulse_width_slide 0
                             mod_pulse_width_slide_shape 1
                             mod_pulse_width_slide_curve 0
                             mod_phase_offset 0
                             mod_wave 1
                             mod_invert_wave 0
                             out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide note_slide_curve note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               0.8
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)
         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide mod_range_slide_curve mod_range_slide_shape)

         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         note                    (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps note)

         cutoff-freq             (midicps cutoff)
         snd                     (saw freq)
         snd                     (lpf snd cutoff-freq)
         snd                     (normalizer snd)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))



 (defsynth sonic-pi-mod_dsaw [note 52
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
                              attack 0.
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
                              mod_phase 0.25
                              mod_phase_slide 0
                              mod_phase_slide_shape 1
                              mod_phase_slide_curve 5
                              mod_range 5
                              mod_range_slide 0
                              mod_range_slide_shape 1
                              mod_range_slide_curve 0
                              mod_pulse_width 0.5
                              mod_pulse_width_slide 0
                              mod_pulse_width_slide_shape 1
                              mod_pulse_width_slide_curve 0
                              mod_phase_offset 0
                              mod_wave 1
                              mod_invert_wave 0
                              detune 0.1
                              detune_slide 0
                              detune_slide_shape 1
                              detune_slide_curve 0
                              out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide note_slide_curve note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               1.3
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         detune                  (varlag detune detune_slide detune_slide_curve detune_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)

         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide)

         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps mod-note)
         cutoff-freq             (midicps cutoff)
         detune-freq             (midicps (+ mod-note detune))
         snd                     (mix (saw [freq detune-freq]))
         snd                     (lpf snd cutoff-freq)
         snd                     (normalizer snd)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


 (defsynth sonic-pi-mod_sine [note 52
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
                              mod_phase 0.25
                              mod_phase_slide 0
                              mod_phase_slide_shape 1
                              mod_phase_slide_curve 0
                              mod_range 5
                              mod_range_slide 0
                              mod_range_slide_shape 1
                              mod_range_slide_curve 0
                              mod_pulse_width 0.5
                              mod_pulse_width_slide 0
                              mod_pulse_width_slide_shape 1
                              mod_pulse_width_slide_curve 0
                              mod_phase_offset 0
                              mod_wave 1
                              mod_invert_wave 0
                              out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide  note_slide_curve  note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               1
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff-freq             (midicps cutoff)
         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)
         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide mod_range_slide_curve mod_range_slide_shape)

         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps mod-note)

         snd                     (sin-osc freq)
         snd                     (lpf snd cutoff-freq)
         snd                     (normalizer snd)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


 (defsynth sonic-pi-mod_tri [note 52
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
                             mod_phase 0.25
                             mod_phase_slide 0
                             mod_phase_slide_shape 1
                             mod_phase_slide_curve 0
                             mod_range 5
                             mod_range_slide 0
                             mod_range_slide_shape 1
                             mod_range_slide_curve 0
                             mod_pulse_width 0.5
                             mod_pulse_width_slide 0
                             mod_pulse_width_slide_shape 1
                             mod_pulse_width_slide_curve 0
                             mod_phase_offset 0
                             mod_wave 1
                             mod_invert_wave 0
                             out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide note_slide_curve note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               1.5
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)

         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)
         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide mod_range_slide_curve mod_range_slide_shape)

         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps mod-note)
         cutoff-freq             (midicps cutoff)

         snd                     (lf-tri freq)
         snd                     (lpf snd cutoff-freq)
         snd                     (normalizer snd)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp))))

 (defsynth sonic-pi-mod_pulse [note 52
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
                               mod_phase 0.25
                               mod_phase_slide 0
                               mod_phase_slide_shape 1
                               mod_phase_slide_curve 0
                               mod_range 5
                               mod_range_slide 0
                               mod_range_slide_shape 1
                               mod_range_slide_curve 0
                               mod_pulse_width 0.5
                               mod_pulse_width_slide 0
                               mod_pulse_width_slide_shape 1
                               mod_pulse_width_slide_curve 0
                               mod_phase_offset 0
                               mod_wave 1
                               mod_invert_wave 0
                               pulse_width 0.5
                               pulse_width_slide 0
                               pulse_width_slide_shape 1
                               pulse_width_slide_curve 0
                               out_bus 0]
   (let [decay_level             (select:kr (= -1 decay_level) [decay_level sustain_level])
         note                    (varlag note note_slide note_slide_curve note_slide_shape)
         amp                     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge               0.8
         pan                     (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff                  (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         mod_phase               (varlag mod_phase mod_phase_slide mod_phase_slide_curve mod_phase_slide_shape)
         mod_rate                (/ 1 mod_phase)
         mod_range               (varlag mod_range mod_range_slide mod_range_slide_curve mod_range_slide_shape)
         pulse_width             (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         mod_pulse_width         (varlag mod_pulse_width mod_pulse_width_slide mod_pulse_width_slide_curve mod_pulse_width_slide_shape)

         min_note                note
         max_note                (+ mod_range note)

         mod_double_phase_offset (* 2 mod_phase_offset)
         ctl-wave                (select:kr mod_wave [(* -1 (lf-saw:kr mod_rate (+ mod_double_phase_offset 1)))
                                                      (- (* 2 (lf-pulse:kr mod_rate mod_phase_offset mod_pulse_width)) 1)
                                                      (lf-tri:kr mod_rate (+ mod_double_phase_offset 1))
                                                      (sin-osc:kr mod_rate (* (+ mod_phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul            (- (* 2 (> mod_invert_wave 0)) 1)
         ctl-wave                (* -1 ctl-wave ctl-wave-mul)
         mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
         freq                    (midicps mod-note)
         cutoff-freq             (midicps cutoff)

         snd                     (pulse freq pulse_width)
         snd                     (lpf snd cutoff-freq)
         snd                     (normalizer snd)
         env                     (env-gen (core/shaped-adsr attack decay sustain release attack_level decay_level sustain_level env_curve) :action FREE)]
     (out out_bus (pan2 (* amp-fudge env snd) pan amp)))))



 (comment
   (core/save-synthdef sonic-pi-beep)
   (core/save-synthdef sonic-pi-saw)
   (core/save-synthdef sonic-pi-tri)
   (core/save-synthdef sonic-pi-pulse)
   (core/save-synthdef sonic-pi-subpulse)
   (core/save-synthdef sonic-pi-square)
   (core/save-synthdef sonic-pi-dsaw)
   (core/save-synthdef sonic-pi-dtri)
   (core/save-synthdef sonic-pi-dpulse)
   (core/save-synthdef sonic-pi-fm)


   (core/save-synthdef sonic-pi-mod_fm)
   (core/save-synthdef sonic-pi-mod_saw)
   (core/save-synthdef sonic-pi-mod_dsaw)
   (core/save-synthdef sonic-pi-mod_sine)
   (core/save-synthdef sonic-pi-mod_tri)
   (core/save-synthdef sonic-pi-mod_pulse)
   )
