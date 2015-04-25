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

(ns sonic-pi.synths.basic
  (:use [overtone.live])
  (:require [sonic-pi.synths.core]))


(do
  (def dull-partials
    [
     0.56
     1.19
     ;;   1.71
     ;;   2.74
     3
     3.76
     ])

  ;; http://www.soundonsound.com/sos/Aug02/articles/synthsecrets0802.asp
  ;; (fig 8)
  (def partials
    [
     1
     4
     ;;   3
     ;;   4.2
     ;;   5.4
     6.8
     ])

  ;; we make a bell by combining a set of sine waves at the given
  ;; proportions of the frequency. Technically not really partials
  ;; as for the 'pretty bell' I stuck mainly with harmonics.
  ;; Each partial is mixed down proportional to its number - so 1 is
  ;; louder than 6. Higher partials are also supposed to attenuate
  ;; quicker but setting the release didn't appear to do much.


  (defcgen bell-partials
    "Bell partial generator"
    [freq     {:default 440 :doc "The fundamental frequency for the partials"}
     attack   {:default 0.01}
     decay    {:default 0}
     sustain  {:default 0}
     release  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
     attack-level {:default 1}
     sustain-level {:default 1}
     partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
    "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
    (:ar
     (apply +
            (map
             (fn [partial proportion]
               (let [amp      (/ proportion 2)
                     env      (env-gen (envelope [0 attack-level sustain-level sustain-level 0] [attack (* decay proportion) (* sustain proportion) (* release proportion)]) :level-scale amp)
                     overtone (* partial freq)]
                 (* env (sin-osc overtone))))
             partials               ;; current partial
             (iterate #(/ % 2) 1.0) ;; proportions (1.0  0.5 0.25)  etc
             ))))

  (without-namespace-in-synthdef
   (defsynth sonic-pi-dull_bell [note 52
                                 note_slide 0
                                 note_slide_shape 5
                                 note_slide_curve 0
                                 amp 1
                                 amp_slide 0
                                 ;;weirdly, having a shape of 2 craps out scsynth!
                                 amp_slide_shape 5
                                 amp_slide_curve 0
                                 pan 0
                                 pan_slide 0
                                 pan_slide_shape 5
                                 pan_slide_curve 0
                                 attack 0.01
                                 decay 0
                                 sustain 0
                                 release 1.0
                                 attack_level 1
                                 sustain_level 1
                                 env_curve 2
                                 out_bus 0]
     (let [note (varlag note note_slide note_slide_curve note_slide_shape)
           amp  (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan  (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq (midicps note)
           snd  (* amp (bell-partials freq attack decay sustain release attack_level sustain_level dull-partials))]
       (detect-silence snd :action FREE)
       (out out_bus (pan2 snd pan))))

   (defsynth sonic-pi-pretty_bell [note 52
                                   note_slide 0
                                   note_slide_shape 5
                                   note_slide_curve 0
                                   amp 1
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
                                   release 1.3
                                   attack_level 1
                                   sustain_level 1
                                   env_curve 2
                                   out_bus 0]
     (let [note (varlag note note_slide note_slide_curve note_slide_shape)
           amp  (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan  (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq (midicps note)
           snd  (* amp (bell-partials freq attack decay sustain release attack_level sustain_level partials))]
       (detect-silence snd :action FREE)
       (out out_bus (pan2 snd pan))))

   (defsynth sonic-pi-beep [note 52
                            note_slide 0
                            note_slide_shape 5
                            note_slide_curve 0
                            amp 1
                            amp_slide 0
                            amp_slide_shape 5
                            amp_slide_curve 0
                            pan 0
                            pan_slide 0
                            pan_slide_shape 5
                            pan_slide_curve 0
                            attack 0
                            decay 0
                            sustain 0
                            release 0.2
                            attack_level 1
                            sustain_level 1
                            env_curve 2
                            out_bus 0]
     (let [note      (varlag note note_slide note_slide_curve note_slide_shape)
           amp       (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge 1
           pan       (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq      (midicps note)
           snd       (sin-osc freq)
           env       (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-pulse [note 52
                             note_slide 0
                             note_slide_shape 5
                             note_slide_curve 0
                             amp 1
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
                             sustain_level 1
                             env_curve 2
                             cutoff 100
                             cutoff_slide 0
                             cutoff_slide_shape 5
                             cutoff_slide_curve 0
                             pulse_width 0.5
                             pulse_width_slide 0
                             pulse_width_slide_shape 5
                             pulse_width_slide_curve 0
                             out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   0.8
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           pulse_width (varlag pulse_width pulse_width_slide pulse_width_slide_shape pulse_width_slide_curve)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           snd         (pulse freq pulse_width)
           snd         (lpf snd cutoff-freq)
           snd         (normalizer snd)
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))

   (defsynth sonic-pi-square [note 52
                              note_slide 0
                              note_slide_shape 5
                              note_slide_curve 0
                              amp 1
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
                              sustain_level 1
                              env_curve 2
                              cutoff 100
                              cutoff_slide 0
                              cutoff_slide_shape 5
                              cutoff_slide_curve 0
                              out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   0.8
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           snd         (pulse freq 0.5)
           snd         (lpf snd cutoff-freq)
           snd         (normalizer snd)
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-saw [note 52
                           note_slide 0
                           note_slide_shape 5
                           note_slide_curve 0
                           amp 1
                           amp_slide 0
                           amp_slide_shape 5
                           amp_slide_curve 0
                           pan 0
                           pan_slide 0
                           pan_slide_shape 5
                           pan_slide_curve 0
                           attack 0.1
                           decay 0
                           sustain 0
                           release 0.3
                           attack_level 1
                           sustain_level 1
                           env_curve 2
                           cutoff 100
                           cutoff_slide 0
                           cutoff_slide_shape 5
                           cutoff_slide_curve 0
                           out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   0.8
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           snd         (normalizer (lpf (saw freq) cutoff-freq))
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-tri [note 52
                           note_slide 0
                           note_slide_shape 5
                           note_slide_curve 0
                           amp 1
                           amp_slide 0
                           amp_slide_shape 5
                           amp_slide_curve 0
                           pan 0
                           pan_slide 0
                           pan_slide_shape 5
                           pan_slide_curve 0
                           attack 0.1
                           decay 0
                           sustain 0
                           release 0.3
                           attack_level 1
                           sustain_level 1
                           env_curve 2
                           cutoff 100
                           cutoff_slide 0
                           cutoff_slide_shape 5
                           cutoff_slide_curve 0
                           out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   1.4
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           snd         (normalizer (lpf (lf-tri freq) cutoff-freq))
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge snd env) pan amp))))


   (defsynth sonic-pi-dsaw [note 52
                            note_slide 0
                            note_slide_shape 5
                            note_slide_curve 0
                            amp 1
                            amp_slide 0
                            amp_slide_shape 5
                            amp_slide_curve 0
                            pan 0
                            pan_slide 0
                            pan_slide_shape 5
                            pan_slide_curve 0
                            attack 0.1
                            decay 0
                            sustain 0
                            release 0.3
                            attack_level 1
                            sustain_level 1
                            env_curve 2
                            cutoff 100
                            cutoff_slide 0
                            cutoff_slide_shape 5
                            cutoff_slide_curve 0
                            detune 0.1
                            detune_slide 0
                            detune_slide_shape 5
                            detune_slide_curve 0
                            out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   1.1
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           detune      (varlag detune detune_slide detune_slide_curve detune_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           detune-freq (midicps (+ note detune))
           snd         (normalizer (lpf (mix (saw [freq detune-freq])) cutoff-freq))
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge snd env) pan amp))))


   (defsynth sonic-pi-fm [note 52
                          note_slide 0
                          note_slide_shape 5
                          note_slide_curve 0
                          amp 1
                          amp_slide 0
                          amp_slide_shape 5
                          amp_slide_curve 0
                          pan 0
                          pan_slide 0
                          pan_slide_shape 5
                          pan_slide_curve 0
                          attack 1
                          decay 0
                          sustain 0
                          release 1
                          attack_level 1
                          sustain_level 1
                          env_curve 2
                          cutoff 100
                          cutoff_slide 0
                          cutoff_slide_shape 5
                          cutoff_slide_curve 0
                          divisor 2.0
                          divisor_slide 0
                          divisor_slide_shape 5
                          divisor_slide_curve 0
                          depth 1.0
                          depth_slide 0
                          depth_slide_shape 5
                          depth_slide_curve 0

                          out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge   0.8
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           divisor     (varlag divisor divisor_slide divisor_slide_curve divisor_slide_shape)
           depth       (varlag depth depth_slide depth_slide_curve depth_slide_shape)
           carrier     (midicps note)
           modulator   (/ carrier divisor)
           cutoff-freq (midicps cutoff)
           env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
           snd         (sin-osc (+ carrier
                                   (* env (* carrier depth) (sin-osc modulator))))
           snd         (lpf snd cutoff-freq)
           ]

       (out out_bus (pan2 (* amp-fudge (* env snd)) pan amp))))


   (defsynth sonic-pi-mod_fm [note 52
                              note_slide 0
                              note_slide_shape 5
                              note_slide_curve 0
                              amp 1
                              amp_slide 0
                              amp_slide_shape 5
                              amp_slide_curve 0
                              pan 0
                              pan_slide 0
                              pan_slide_shape 5
                              pan_slide_curve 0
                              attack 1
                              decay 0
                              sustain 0
                              release 1
                              attack_level 1
                              sustain_level 1
                              env_curve 2
                              cutoff 100
                              cutoff_slide 0
                              cutoff_slide_shape 5
                              cutoff_slide_curve 0
                              mod_phase 1
                              mod_phase_slide 0
                              mod_phase_slide_shape 5
                              mod_phase_slide_curve 0
                              mod_range 5
                              mod_range_slide 0
                              mod_range_slide_shape 5
                              mod_range_slide_curve 0
                              mod_pulse_width 0.5
                              mod_pulse_width_slide 0
                              mod_pulse_width_slide_shape 5
                              mod_pulse_width_slide_curve 0
                              mod_phase_offset 0
                              mod_wave 1
                              mod_invert_wave 0
                              divisor 2.0
                              divisor_slide 0
                              divisor_slide_shape 5
                              divisor_slide_curve 0
                              depth 1.0
                              depth_slide 0
                              depth_slide_shape 5
                              depth_slide_curve 0
                              out_bus 0]
     (let [note                    (varlag note note_slide note_slide_curve note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           note                    (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps note)
           cutoff-freq             (midicps cutoff)
           divisor                 (varlag divisor divisor_slide divisor_slide_curve divisor_slide_shape)
           depth                   (varlag depth depth_slide depth_slide_curve depth_slide_shape)
           carrier                 freq
           modulator               (/ carrier divisor)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
           snd                     (sin-osc (+ carrier
                                               (* env (* carrier depth) (sin-osc modulator))))
           snd                     (lpf snd cutoff-freq)
           ]

       (out out_bus (pan2 (* amp-fudge (* env snd)) pan amp))))


   (defsynth sonic-pi-mod_saw [note 52
                               note_slide 0
                               note_slide_shape 5
                               note_slide_curve 0
                               amp 1
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
                               sustain_level 1
                               env_curve 2
                               cutoff 100
                               cutoff_slide 0
                               cutoff_slide_shape 5
                               cutoff_slide_curve 0
                               mod_phase 1
                               mod_phase_slide 0
                               mod_phase_slide_shape 5
                               mod_phase_slide_curve 0

                               mod_range 5
                               mod_range_slide 0
                               mod_range_slide_shape 5
                               mod_range_slide_curve 0
                               mod_pulse_width 0.5
                               mod_pulse_width_slide 0
                               mod_pulse_width_slide_shape 5
                               mod_pulse_width_slide_curve 0
                               mod_phase_offset 0
                               mod_wave 1
                               mod_invert_wave 0
                               out_bus 0]
     (let [note                    (varlag note note_slide note_slide_curve note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           note                    (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps note)

           cutoff-freq             (midicps cutoff)
           snd                     (saw freq)
           snd                     (lpf snd cutoff-freq)
           snd                     (normalizer snd)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-mod_dsaw [note 52
                                note_slide 0
                                note_slide_shape 5
                                note_slide_curve 0
                                amp 1
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
                                sustain_level 1
                                env_curve 2
                                cutoff 100
                                cutoff_slide 0
                                cutoff_slide_shape 5
                                cutoff_slide_curve 0
                                mod_phase 1
                                mod_phase_slide 0
                                mod_phase_slide_shape 5
                                mod_phase_slide_curve 5
                                mod_range 5
                                mod_range_slide 0
                                mod_range_slide_shape 5
                                mod_range_slide_curve 0
                                mod_pulse_width 0.5
                                mod_pulse_width_slide 0
                                mod_pulse_width_slide_shape 5
                                mod_pulse_width_slide_curve 0
                                mod_phase_offset 0
                                mod_wave 0
                                mod_invert_wave 0
                                detune 0.1
                                detune_slide 0
                                detune_slide_shape 5
                                detune_slide_curve 0
                                out_bus 0]
     (let [note                    (varlag note note_slide note_slide_curve note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps mod-note)
           cutoff-freq             (midicps cutoff)
           detune-freq             (midicps (+ mod-note detune))
           snd                     (mix (saw [freq detune-freq]))
           snd                     (lpf snd cutoff-freq)
           snd                     (normalizer snd)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-mod_sine [note 52
                                note_slide 0
                                note_slide_shape 5
                                note_slide_curve 0
                                amp 1
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
                                sustain_level 1
                                env_curve 2
                                cutoff 100
                                cutoff_slide 0
                                cutoff_slide_shape 5
                                cutoff_slide_curve 0
                                mod_phase 1
                                mod_phase_slide 0
                                mod_phase_slide_shape 5
                                mod_phase_slide_curve 0
                                mod_range 5
                                mod_range_slide 0
                                mod_range_slide_shape 5
                                mod_range_slide_curve 0
                                mod_pulse_width 0.5
                                mod_pulse_width_slide 0
                                mod_pulse_width_slide_shape 5
                                mod_pulse_width_slide_curve 0
                                mod_phase_offset 0
                                mod_wave 0
                                mod_invert_wave 0
                                out_bus 0]
     (let [note                    (varlag note note_slide  note_slide_curve  note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps mod-note)

           snd                     (sin-osc freq)
           snd                     (lpf snd cutoff-freq)
           snd                     (normalizer snd)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-mod_tri [note 52
                               note_slide 0
                               note_slide_shape 5
                               note_slide_curve 0
                               amp 1
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
                               sustain_level 1
                               env_curve 2
                               cutoff 100
                               cutoff_slide 0
                               cutoff_slide_shape 5
                               cutoff_slide_curve 0
                               mod_phase 1
                               mod_phase_slide 0
                               mod_phase_slide_shape 5
                               mod_phase_slide_curve 0
                               mod_range 5
                               mod_range_slide 0
                               mod_range_slide_shape 5
                               mod_range_slide_curve 0
                               mod_pulse_width 0.5
                               mod_pulse_width_slide 0
                               mod_pulse_width_slide_shape 5
                               mod_pulse_width_slide_curve 0
                               mod_phase_offset 0
                               mod_wave 0
                               mod_invert_wave 0
                               out_bus 0]
     (let [note                    (varlag note note_slide note_slide_curve note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps mod-note)
           cutoff-freq             (midicps cutoff)

           snd                     (lf-tri freq)
           snd                     (lpf snd cutoff-freq)
           snd                     (normalizer snd)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))


   (defsynth sonic-pi-mod_pulse [note 52
                                 note_slide 0
                                 note_slide_shape 5
                                 note_slide_curve 0
                                 amp 1
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
                                 sustain_level 1
                                 env_curve 2
                                 cutoff 100
                                 cutoff_slide 0
                                 cutoff_slide_shape 5
                                 cutoff_slide_curve 0
                                 mod_phase 1
                                 mod_phase_slide 0
                                 mod_phase_slide_shape 5
                                 mod_phase_slide_curve 0
                                 mod_range 5
                                 mod_range_slide 0
                                 mod_range_slide_shape 5
                                 mod_range_slide_curve 0
                                 mod_pulse_width 0.5
                                 mod_pulse_width_slide 0
                                 mod_pulse_width_slide_shape 5
                                 mod_pulse_width_slide_curve 0
                                 mod_phase_offset 0
                                 mod_wave 0
                                 mod_invert_wave 0
                                 pulse_width 0.5
                                 pulse_width_slide 0
                                 pulse_width_slide_shape 5
                                 pulse_width_slide_curve 0
                                 out_bus 0]
     (let [note                    (varlag note note_slide note_slide_curve note_slide_shape)
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
           ctl-wave                (* ctl-wave ctl-wave-mul)
           mod-note                (lin-lin ctl-wave -1 1 min_note max_note)
           freq                    (midicps mod-note)
           cutoff-freq             (midicps cutoff)

           snd                     (pulse freq pulse_width)
           snd                     (lpf snd cutoff-freq)
           snd                     (normalizer snd)
           env                     (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)]
       (out out_bus (pan2 (* amp-fudge env snd) pan amp))))

   )

  (comment
    (save-to-pi sonic-pi-dull_bell)
    (save-to-pi sonic-pi-pretty_bell)
    (save-to-pi sonic-pi-beep)
    (save-to-pi sonic-pi-saw)
    (save-to-pi sonic-pi-tri)
    (save-to-pi sonic-pi-pulse)
    (save-to-pi sonic-pi-square)
    (save-to-pi sonic-pi-dsaw)
    (save-to-pi sonic-pi-fm)

    (save-to-pi sonic-pi-mod_fm)
    (save-to-pi sonic-pi-mod_saw)
    (save-to-pi sonic-pi-mod_dsaw)
    (save-to-pi sonic-pi-mod_sine)
    (save-to-pi sonic-pi-mod_tri)
    (save-to-pi sonic-pi-mod_pulse)
    ))
