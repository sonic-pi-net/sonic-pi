;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;
;; Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, distribution,
;; and distribution of modified versions of this work as long as this
;; notice is included.
;;++

;; This file uses Overtone to compile the synths from Clojure to
;; SuperCollider compatible binary files. Overtone is Sonic Pi's big
;; brother. See: http://overtone.github.io

(ns sp
  (:use [overtone.live])

  (:require [clojure.string :as str]
            [overtone.sc.dyn-vars :as dvars])
  )

;; Utility functions (for creating and storing synthdefs)

(defn save-synthdef [sdef folder]
  (let [path (str folder "/" (last (str/split (-> sdef :sdef :name) #"/")) ".scsyndef") ]
    (overtone.sc.machinery.synthdef/synthdef-write (:sdef sdef) path)
    path))

(defn save-to-pi [sdef]
  (save-synthdef sdef "/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/"))


;; Main mixer

(do
  (without-namespace-in-synthdef
   (defsynth sonic-pi-mixer [in_bus 0 amp 1 safe-recovery-time 3 hpf_freq 0 hpf_pass_thru 1 lpf_freq 0 lpf_pass_thru 1 force_mono 0, invert_stereo 0]
     (let [l        (in in_bus 1)
           r        (in (+ in_bus 1))
           amp      (lag-ud amp 0 0.02)
           l        (* amp l)
           r        (* amp r)

           l        (select:ar hpf_pass_thru
                               [(hpf l hpf_freq)
                                l])
           r        (select:ar hpf_pass_thru
                               [(hpf r hpf_freq)
                                r])

           l        (select:ar lpf_pass_thru
                               [(lpf l lpf_freq)
                                l])
           r        (select:ar lpf_pass_thru
                               [(lpf r lpf_freq)
                                r])

           l   (select:ar force_mono
                          [l
                           (/ (+ l r) 2)])
           r   (select:ar force_mono
                          [r
                           (/ (+ l r) 2)])

           l2 (select:ar invert_stereo
                         [l
                          r])

           r2 (select:ar invert_stereo
                         [r
                          l])
           safe-snd (limiter [l2 r2] 0.99 0.01)]
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
                                pan 0
                                pan_slide 0
                                input 0
                                out_bus 0]
     (let [snd (sound-in 0)]
       (out out_bus (pan2 snd pan amp)))))


  (comment
    (save-to-pi sonic-pi-sound_in)
    (save-to-pi sonic-pi-mixer)
    (save-to-pi sonic-pi-basic_mixer)
    (save-to-pi sonic-pi-recorder)))


;; Simple Trigger synths

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

;; Sample playback synths

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
    rate_slide 0
    rate_slide_shape 5
    rate_slide_curve 0
    norm 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         rate        (varlag rate rate_slide rate_slide_curve rate_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         use-filter  (> cutoff 0)
         rate        (* rate (buf-rate-scale buf))
         snd         (play-buf 1 buf rate :action FREE)
         snd         (select use-filter [snd (rlpf snd cutoff-freq res)])
         snd         (select norm [snd (normalizer snd)])]
     (out out_bus (pan2 snd pan  amp))))

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
    rate_slide 0
    rate_slide_shape 5
    rate_slide_curve 0
    norm 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         rate          (varlag rate rate_slide rate_slide_curve rate_slide_shape)
         rate          (* rate (buf-rate-scale buf))
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq   (midicps cutoff)
         use-filter    (> cutoff 0)
         [snd-l snd-r] (play-buf 2 buf rate :action FREE)
         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])
         snd-l         (select norm [snd-l (normalizer snd-l)])
         snd-r         (select norm [snd-r (normalizer snd-r)])
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
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
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
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (buf-rd 1 buf phase)
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
    sustain_level 1
    env_curve 2
    rate 1
    start 0
    finish 1
    norm 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
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
         env           (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)

         [snd-l snd-r] (buf-rd 2 buf phase)
         snd-l         (select use-filter [snd-l (rlpf snd-l cutoff-freq res)])
         snd-r         (select use-filter [snd-r (rlpf snd-r cutoff-freq res)])
         snd-l         (select norm [snd-l (normalizer snd-l)])
         snd-r         (select norm [snd-r (normalizer snd-r)])
         snd-l         (* env snd-l)
         snd-r         (* env snd-r)
         snd           (balance2 snd-l snd-r pan amp)]

      (out out_bus snd)))

 (comment
   (save-to-pi sonic-pi-mono_player)
   (save-to-pi sonic-pi-stereo_player)
   (save-to-pi sonic-pi-basic_mono_player)
   (save-to-pi sonic-pi-basic_stereo_player)))


(without-namespace-in-synthdef

 (defsynth sonic-pi-bnoise
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0
    sustain 0
    decay 0
    release 2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff 120
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.2
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (brown-noise) cutoff-freq res)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-pnoise
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0
    sustain 0
    decay 0
    release 2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff 120
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp_fudge   3
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (pink-noise) cutoff-freq res)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (* amp_fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-gnoise
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0
    sustain 0
    decay 0
    release 2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff 120
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp_fudge   1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (gray-noise) cutoff-freq res)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (* amp_fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-noise
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0
    sustain 0
    decay 0
    release 2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff 120
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.9
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (white-noise) cutoff-freq res)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))


 (defsynth sonic-pi-cnoise
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan 0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack 0
    sustain 0
    decay 0
    release 2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff 120
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.1
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.6
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)
         snd         (rlpf (clip-noise) cutoff-freq res)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         snd         (* amp-fudge snd env)]

     (out out_bus (pan2 snd pan amp))))

 (comment
   (save-to-pi sonic-pi-noise)
   (save-to-pi sonic-pi-pnoise)
   (save-to-pi sonic-pi-bnoise)
   (save-to-pi sonic-pi-gnoise)
   (save-to-pi sonic-pi-cnoise)
   )
 )


(without-namespace-in-synthdef

 (defsynth sonic-pi-tb303
   "A simple clone of the sound of a Roland TB-303 bass synthesizer."
   [note     52                        ; midi note value input
    note_slide 0
    note_slide_shape 5
    note_slide_curve 0
    amp      1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    pan      0
    pan_slide 0
    pan_slide_shape 5
    pan_slide_curve 0
    attack   0.01
    sustain  0
    decay 0
    release  2
    attack_level 1
    sustain_level 1
    env_curve 2
    cutoff   80
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    cutoff_attack 0.01
    cutoff_sustain 0
    cutoff_decay 0
    cutoff_release 2
    cutoff_min 30
    cutoff_min_slide 0
    cutoff_min_slide_shape 5
    cutoff_min_slide_curve 0
    cutoff_attack_level 1
    cutoff_sustain_level 1
    cutoff_env_curve 2
    res      0.1                       ; rlpf resonance
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    wave     0                         ; 0=saw, 1=pulse, 2=tri
    pulse_width 0.5                    ; only for pulse wave
    pulse_width_slide 0
    pulse_width_slide_shape 5
    pulse_width_slide_curve 0
    out_bus  0]
   (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff_min  (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         pulse_width (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         freq        (midicps note)
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         filt-env    (env-gen (env-adsr-ng cutoff_attack cutoff_decay cutoff_sustain cutoff_release cutoff_attack_level cutoff_sustain_level cutoff_env_curve))

         snd         (rlpf (select:ar wave [(saw freq)
                                            (pulse freq pulse_width)
                                            (* 2 (lf-tri freq))])

                           (+ (midicps cutoff_min) (* filt-env (midicps cutoff) ))
                           res)

         snd         (* amp-fudge env snd)]
     (out out_bus (pan2 snd pan amp))))

 (defsynth sonic-pi-supersaw [note 52
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
                              cutoff 130
                              cutoff_slide 0
                              cutoff_slide_shape 5
                              cutoff_slide_curve 0
                              res 0.3
                              res_slide 0
                              res_slide_shape 5
                              res_slide_curve 0
                              out_bus 0]
   (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   0.9
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
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
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         output      (* amp-fudge env output)
         output      (pan2 output pan amp)]
     (out out_bus output)))

 (defsynth sonic-pi-zawa [note 52
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
                          release 1
                          attack_level 1
                          sustain_level 1
                          env_curve 2
                          cutoff 100
                          cutoff_slide 0
                          cutoff_slide_shape 5
                          cutoff_slide_curve 0
                          res 0.1
                          res_slide 0
                          res_slide_shape 5
                          res_slide_curve 0
                          phase 1
                          phase_slide 0
                          phase_slide_shape 5
                          phase_slide_curve 0
                          phase_offset 0
                          wave 0
                          disable_wave 0
                          invert_wave 0
                          pulse_width 0.5
                          pulse_width_slide 0
                          pulse_width_slide_shape 5
                          pulse_width_slide_curve 0
                          range 1
                          range_slide 0
                          range_slide_shape 5
                          range_slide_curve 0
                          out_bus 0]
   (let [note                (varlag note note_slide note_slide_curve note_slide_shape)
         amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge           0.5
         pan                 (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         range               (varlag range range_slide range_slide_curve range_slide_shape)
         wob_rate            (/ 1 phase)
         cutoff              (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         freq                (midicps note)
         cutoff              (midicps cutoff)
         double_phase_offset (* 2 phase_offset)
         rate                (/ 1 phase)
         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
         ctl-wave            (* ctl-wave ctl-wave-mul)

         saw-freq            (select:kr disable_wave [(midicps (lin-lin ctl-wave -1 1 note (+ note range)))
                                                      (midicps (+ note range))])
         snd                 (rlpf (sync-saw
                                    freq
                                    saw-freq)
                                   cutoff
                                   res)
         env                 (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
         output              (* amp-fudge env snd)
         output              (pan2 output pan amp)]
     (out out_bus output)))


 (defsynth sonic-pi-prophet
   "The Prophet Speaks (page 2)

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
    cutoff 110
    cutoff_slide 0
    cutoff_slide_shape 5
    cutoff_slide_curve 0
    res 0.3
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    out_bus 0 ]

   (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
         amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         amp-fudge   1.5
         pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
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
         env         (env-gen (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)

         snd         (rlpf (* env snd snd) cutoff-freq res)
         snd         (* amp-fudge env snd)
         snd         (leak-dc snd)
         snd         (pan2 snd pan amp)]

     (out out_bus snd)))

 (comment
   (save-to-pi sonic-pi-tb303)
   (save-to-pi sonic-pi-supersaw)
   (save-to-pi sonic-pi-prophet)
   (save-to-pi sonic-pi-zawa)



   ))

(without-namespace-in-synthdef
    (defsynth sonic-pi-dark_ambience
     [note 52
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
      release 4
      attack_level 1
      sustain_level 1
      env_curve 2

      cutoff 110
      cutoff_slide 0
      cutoff_slide_shape 5
      cutoff_slide_curve 0
      res 0.3
      res_slide 0
      res_slide_shape 5
      res_slide_curve 0

      detune1 12
      detune1_slide 0
      detune1_slide_shape 5
      detune1_slide_curve 0

      detune2 24
      detune2_slide 0
      detune2_slide_shape 5
      detune2_slide_curve 0

      noise 0
      ring 0.2
      room 70
      reverb_time 100
      out_bus 0
      ]
     (let [
           note          (varlag note note_slide note_slide_curve note_slide_shape)
           amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           amp-fudge     1
           pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res           (varlag res res_slide res_slide_curve res_slide_shape)
           detune1       (varlag detune1 detune1_slide detune1_slide_curve detune1_slide_shape)
           detune2       (varlag detune2 detune2_slide detune2_slide_curve detune2_slide_shape)

           freq          (midicps note)
           freq2         (midicps (+ note detune1))
           freq3         (midicps (+ note detune2))
           cutoff-freq   (midicps cutoff)
           room          (max 0.1 (min 300 (abs room))) ;; stops synth killing scsynth
           env           (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)

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
      note_slide_shape 5
      note_slide_curve 0

      pan 0
      pan_slide 0
      pan_slide_shape 5
      pan_slide_curve 0

      amp 1
      amp_slide 0
      amp_slide_shape 5
      amp_slide_curve 0

      attack 0
      decay 0
      sustain 0
      release 0.1
      attack_level 1
      sustain_level 1
      env_curve 2

      cutoff 100
      cutoff_slide 0
      cutoff_slide_shape 5
      cutoff_slide_curve 0
      res 0.3
      res_slide 0
      res_slide_shape 5
      res_slide_curve 0

      noise 0
      norm 1

      out_bus 0]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           amp-fudge 15
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res         (varlag res res_slide res_slide_curve res_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)

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
      note_slide_shape 5
      note_slide_curve 0

      pan 0
      pan_slide 0
      pan_slide_shape 5
      pan_slide_curve 0

      amp 1
      amp_slide 0
      amp_slide_shape 5
      amp_slide_curve 0

      attack 0.1
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
      res 1
      res_slide 0
      res_slide_shape 5
      res_slide_curve 0
]
     (let [note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res         (varlag res res_slide res_slide_curve res_slide_shape)
           freq        (midicps note)
           cutoff-freq (midicps cutoff)
           amp-fudge   3
           env         (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :action FREE)
           snd         (lpf (mix [(saw (* 0.25 freq)) (sin-osc (* 1.01 freq))]))
           snd         (pitch-shift snd 0.5 1 0 0.001)
           snd         (rlpf snd cutoff-freq res)
]
       (out out_bus (* env (pan2 (* amp-fudge snd) pan amp)))))

    (comment
      (save-to-pi sonic-pi-dark_ambience)
      (save-to-pi sonic-pi-hollow)
      (save-to-pi sonic-pi-growl)))

;;FX
(without-namespace-in-synthdef
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


 (defsynth sonic-pi-fx_replace_bitcrusher
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         sample_rate   (varlag sample_rate sample_rate_slide sample_rate_slide_curve sample_rate_slide_shape)
         bits          (varlag bits bits_slide bits_slide_curve bits_slide_shape)
         [in-l in-r]   (* pre_amp (in:ar out_bus 2))
         [new-l new-r] (decimator [in-l in-r] sample_rate bits)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_reverb
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
    out_bus 0]
   (let [amp     (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix     (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         room    (varlag room room_slide room_slide_curve room_slide_shape)
         damp    (varlag damp damp_slide damp_slide_curve damp_slide_shape)
         [l r]   (* pre_amp (in:ar out_bus 2))
         snd     (* amp (free-verb2 l r mix room damp))]
     (replace-out out_bus snd)))


 (defsynth sonic-pi-fx_level
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp (varlag amp amp_slide amp_slide_curve amp_slide_shape)]
     (out out_bus (* amp (in in_bus 2)))))


 (defsynth sonic-pi-fx_replace_level
   [amp 1
    amp_slide 0
    amp_slide_shape 5
    amp_slide_curve 0
    out_bus 0]
   (let [amp (varlag amp amp_slide  amp_slide_curve amp_slide_shape)]
     (replace-out out_bus (* amp (in out_bus 2)))))


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


 (defsynth sonic-pi-fx_replace_echo
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         decay         (varlag decay decay_slide decay_slide_curve decay_slide_shape)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (+ [in-l in-r] (comb-n [in-l in-r] max_phase phase decay))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         amp_min             (varlag amp_min amp_min_slide amp_min_slide_curve amp_min_slide_shape)
         amp_max             (varlag amp_max amp_max_slide amp_max_slide_curve amp_max_slide_shape)
         rate                (/ 1 phase)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         double_phase_offset (* 2 phase_offset)

         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)

         slice-amp           (* ctl-wave ctl-wave-mul)
         slice-amp           (lin-lin slice-amp -1 1 amp_min amp_max)
         [in-l in-r]         (* pre_amp (in in_bus 2))
         [new-l new-r]       (* slice-amp [in-l in-r])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_slicer
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
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         amp_min             (varlag amp_min amp_min_slide amp_min_slide_curve amp_min_slide_shape)
         amp_max             (varlag amp_max amp_max_slide amp_max_slide_curve amp_max_slide_shape)
         rate                (/ 1 phase)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         double_phase_offset (* 2 phase_offset)

         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)

         slice-amp           (* ctl-wave ctl-wave-mul)
         slice-amp           (lin-lin slice-amp -1 1 amp_min amp_max)
         [in-l in-r]         (* pre_amp (in out_bus 2))
         [new-l new-r]       (* slice-amp [in-l in-r])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
    phase_offset 0.5
    wave 0                              ;0=saw, 1=pulse, 2=tri, 3=sin
    invert_wave 0                       ;0=normal wave, 1=inverted wave
    pulse_width 0.5                     ; only for pulse wave
    pulse_width_slide 0                 ; only for pulse wave
    pulse_width_slide_shape 5           ; only for pulse wa_shape 5
    pulse_width_slide_curve 0           ; only for pulse wa_curve 0
    filter 0                            ;0=rlpf, 1=rhpf
    in_bus 0
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         rate                (/ 1 phase)
         cutoff_min          (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         cutoff_max          (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         res                 (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff_min          (midicps cutoff_min)
         cutoff_max          (midicps cutoff_max)
         double_phase_offset (* 2 phase_offset)

         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)

         cutoff-freq         (lin-exp:kr (* ctl-wave-mul ctl-wave) -1 1 cutoff_min cutoff_max)

         [in-l in-r]         (* pre_amp (in in_bus 2))

         new-l               (select:ar filter [(rlpf in-l cutoff-freq res)
                                                (rhpf in-l cutoff-freq res)])
         new-r               (select:ar filter [(rlpf in-r cutoff-freq res)
                                                (rhpf in-r cutoff-freq res)])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_wobble
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
    phase_offset 0.5
    wave 0                              ;0=saw, 1=pulse, 2=tri, 3=sin
    invert_wave 0                       ;0=normal wave, 1=inverted wave
    pulse_width 0.5                     ; only for pulse wave
    pulse_width_slide 0                 ; only for pulse wave
    pulse_width_slide_shape 5           ; only for pulse wa_shape 5
    pulse_width_slide_curve 0           ; only for pulse wa_curve 0
    filter 0                            ;0=rlpf, 1=rhpf
    out_bus 0]
   (let [amp                 (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix                 (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp             (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         rate                (/ 1 phase)
         cutoff_min          (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         cutoff_max          (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
         pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
         res                 (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff_min          (midicps cutoff_min)
         cutoff_max          (midicps cutoff_max)
         double_phase_offset (* 2 phase_offset)
         ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                              (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                              (lf-tri:kr rate (+ double_phase_offset 1))
                                              (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

         ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)

         cutoff-freq         (lin-exp:kr (* ctl-wave-mul ctl-wave) -1 1 cutoff_min cutoff_max)

         [in-l in-r]         (* pre_amp (in out_bus 2))

         new-l               (select:ar filter [(rlpf in-l cutoff-freq res)
                                                (rhpf in-l cutoff-freq res)])
         new-r               (select:ar filter [(rlpf in-r cutoff-freq res)
                                                (rhpf in-r cutoff-freq res)])
         fin-l               (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r               (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff_min    (midicps cutoff_min)
         cutoff_max    (midicps cutoff_max)
         freq          (lin-exp (sin-osc:kr rate (* (- phase_offset 0.25) (* Math/PI 2))) -1 1 cutoff_min cutoff_max)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rlpf [in-l in-r] freq res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_ixi_techno
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         rate          (/ 1 phase)
         cutoff_min    (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
         cutoff_max    (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff_min    (midicps cutoff_min)
         cutoff_max    (midicps cutoff_max)
         freq          (lin-exp (sin-osc:kr rate (* (- phase_offset 0.25) (* Math/PI 2))) -1 1 cutoff_min cutoff_max)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (rlpf [in-l in-r] freq res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_compressor
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         threshold     (varlag threshold threshold_slide threshold_slide_curve threshold_slide_shape)
         clamp_time    (varlag clamp_time clamp_time_slide clamp_time_slide_curve clamp_time_slide_shape)
         slope_above   (varlag slope_above slope_above_slide slope_above_slide_curve slope_above_slide_shape)
         slope_below   (varlag slope_below slope_below_slide slope_below_slide_curve slope_below_slide_shape)
         relax_time    (varlag relax_time relax_time_slide relax_time_slide_curve relax_time_slide_shape)

         src           (* pre_amp (in out_bus 2))
         [in-l in-r]   src

         control-sig   (/ (+ in-l in-r) 2)

         [new-l new-r] (compander src control-sig threshold
                                  slope_below slope_above
                                  clamp_time relax_time)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rlpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_rlpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (rlpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (rlpf [in-l in-r] cutoff res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_nrlpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (normalizer (rlpf [in-l in-r] cutoff res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (rhpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))


 (defsynth sonic-pi-fx_replace_rhpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (varlag res res_slide res_slide_curve res_slide_shape)
         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (rhpf [in-l in-r] cutoff res)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_nrhpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (normalizer (rhpf [in-l in-r] cutoff res))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_hpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (hpf [in-l in-r] cutoff)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_nhpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (normalizer (hpf [in-l in-r] cutoff))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_lpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (lpf [in-l in-r] cutoff)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_nlpf
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         cutoff        (midicps cutoff)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (normalizer (lpf [in-l in-r] cutoff))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_normaliser
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         level         (varlag level level_slide level_slide_curve level_slide_shape)

         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (normalizer [in-l in-r] level)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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



 (defsynth sonic-pi-fx_replace_distortion
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
    out_bus 0]
   (let [amp           (varlag:kr amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag:kr mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag:kr pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         distort       (varlag:kr distort distort_slide distort_slide_curve distort_slide_shape)
         k             (/ (* 2 distort) (- 1 distort))

         src           (* pre_amp (in out_bus 2))
         [new-l new-r] (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
         [in-l in-r]   src
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))


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


 (defsynth sonic-pi-fx_replace_pan
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
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
         [in-l in-r]   (* pre_amp (in out_bus 2))
         [new-l new-r] (balance2 in-l in-r pan amp)
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (replace-out out_bus [fin-l fin-r])))

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
         freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
         freq          (midicps freq)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (* [in-l in-r] (* (sin-osc freq) mod_amp))
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
         freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
         freq          (midicps freq)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (* [in-l in-r] (sin-osc freq mod_amp))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))

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

         delay-l             (allpass-c  (+ (limiter (* local-l feedback)) in-l)
                                         (/ max_delay 1000)
                                         (max
                                          (mul-add ctl-wave-l (/ depth 1000) (/ delay 1000))
                                          0)
                                         (/ decay 1000))

         delay-r             (allpass-c  (+ (limiter (* local-r feedback)) in-r)
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
   (save-to-pi sonic-pi-fx_bitcrusher)
   (save-to-pi sonic-pi-fx_replace_bitcrusher)
   (save-to-pi sonic-pi-fx_reverb)
   (save-to-pi sonic-pi-fx_replace_reverb)
   (save-to-pi sonic-pi-fx_level)
   (save-to-pi sonic-pi-fx_replace_level)
   (save-to-pi sonic-pi-fx_echo)
   (save-to-pi sonic-pi-fx_replace_echo)
   (save-to-pi sonic-pi-fx_slicer)
   (save-to-pi sonic-pi-fx_replace_slicer)
   (save-to-pi sonic-pi-fx_wobble)
   (save-to-pi sonic-pi-fx_replace_wobble)
   (save-to-pi sonic-pi-fx_ixi_techno)
   (save-to-pi sonic-pi-fx_replace_ixi_techno)
   (save-to-pi sonic-pi-fx_compressor)
   (save-to-pi sonic-pi-fx_replace_compressor)
   (save-to-pi sonic-pi-fx_rlpf)
   (save-to-pi sonic-pi-fx_replace_rlpf)
   (save-to-pi sonic-pi-fx_nrlpf)
   (save-to-pi sonic-pi-fx_replace_nrlpf)
   (save-to-pi sonic-pi-fx_rhpf)
   (save-to-pi sonic-pi-fx_replace_rhpf)
   (save-to-pi sonic-pi-fx_nrhpf)
   (save-to-pi sonic-pi-fx_replace_nrhpf)
   (save-to-pi sonic-pi-fx_hpf)
   (save-to-pi sonic-pi-fx_replace_hpf)
   (save-to-pi sonic-pi-fx_nhpf)
   (save-to-pi sonic-pi-fx_replace_nhpf)
   (save-to-pi sonic-pi-fx_lpf)
   (save-to-pi sonic-pi-fx_replace_lpf)
   (save-to-pi sonic-pi-fx_nlpf)
   (save-to-pi sonic-pi-fx_replace_nlpf)
   (save-to-pi sonic-pi-fx_normaliser)
   (save-to-pi sonic-pi-fx_replace_normaliser)
   (save-to-pi sonic-pi-fx_distortion)
   (save-to-pi sonic-pi-fx_replace_distortion)
   (save-to-pi sonic-pi-fx_pan)
   (save-to-pi sonic-pi-fx_replace_pan)
   (save-to-pi sonic-pi-fx_bpf)
   (save-to-pi sonic-pi-fx_rbpf)
   (save-to-pi sonic-pi-fx_nrbpf)
   (save-to-pi sonic-pi-fx_ring_mod)
   (save-to-pi sonic-pi-fx_octaver)
   (save-to-pi sonic-pi-fx_flanger)
   ))


;; Experimental
(comment
  (do
    ;;TODO FIXME!
    (defsynth sonic-pi-babbling [out_bus 0 x 0 y 50]
      (let [x      (abs x)
            x      (/ x 100000)
            x      (min x 0.05)
            x      (max x 0.005)
            y      (abs y)
            y      (min y 10000)
            y      (max y 200)
            noise  (* 0.003
                      (rhpf (one-pole (* 0.99 (brown-noise)))
                            (+ 500 (* 400 (lpf (* (brown-noise) 14))))
                            x))
            noise  [noise noise]
            noise2 (* 0.005
                      (rhpf (one-pole (* 0.99 (brown-noise)))
                            (+ 1000 (* 800 (lpf (* (brown-noise) 20))))
                            x))
            noise2 [noise2 noise2]
            mixed  (+ noise noise2)
            mixed  (lpf mixed y)]
        (out out_bus (* 0 (mix (* 3 mixed))))))
    (save-to-pi sonic-pi-babbling))

  (do
    (defsynth sonic-pi-woah [note 52 out_bus 0 x 0 y 0]
      (let [freq (midicps note)
            x    (abs x)
            x    (/ x 700)
            x    (min x 15)
            x    (max x 0.5)
            snd  (lpf (sync-saw
                       freq
                       (* (* freq 1.5) (+ 2 (sin-osc:kr x))))
                      1000)]
        (out out_bus (* 0.25 snd))))

    (save-to-pi sonic-pi-woah))


  (do
    (defsynth sonic-pi-arpeg-click [x 10 buf 0 arp-div 2 beat-div 1 out_bus 0]
      (let [x (abs x)
            x (/ x 70)
            x (min x 200)
            x (max x 1)
            tik   (impulse x)
            a-tik (pulse-divider tik arp-div)
            b-tik (pulse-divider tik beat-div)
            cnt   (mod (pulse-count a-tik) (buf-frames 1))
            note  (buf-rd:kr 1 1 cnt)
            freq  (midicps note)
            snd   (white-noise)
            snd   (rhpf snd 2000 0.4)
            snd   (normalizer snd)
            b-env (env-gen (perc 0.01 0.1) b-tik)
            a-env (env-gen (perc 0.01 0.4) a-tik)]
        (out out_bus (* 0.20 (pan2 (+ (* 0.5 snd b-env)
                                      (* (sin-osc freq) a-env)
                                      (* (sin-osc (* 2 freq)) a-env)))))))

    (save-to-pi sonic-pi-arpeg-click) )

  (do
    (defsynth sonic-pi-space_organ [note 24 amp 1 x 0 y 0 out_bus 0]
      (let [freq-shift (/ x 100)
            delay (* -1 (/ x 10000))]
        (out out_bus (pan2  (g-verb (* 0.2 (mix (map #(blip (+ freq-shift (* (midicps (duty:kr % 0 (dseq [note (+ 3 note) (+ 7 note) (+ 12 note) (+ 17 note)] INF))) %2)) (mul-add:kr (lf-noise1:kr 1/2) 3 4)) (+ delay [1 1/4]) [1  8]))) 200 8)))))


    (save-to-pi sonic-pi-space_organ)

    (defsynth sonic-pi-saws [note 52 x 0 y 0 out_bus 0]
      (let [x    (abs x)
            x    (min x 10000)
            x    (max x 50)
            y    (abs y)
            y    (/ y 10000)
            y    (min y 0.3)
            y    (max y 0)
            freq (midicps note)]
        (out out_bus (mix (* 0.15 (normalizer (lpf (saw [freq (+ freq (* freq y))]) x)))))))

    (save-to-pi sonic-pi-saws) )

  (mod_dsaw 52)

  (def s (freesound 18765))





  (defsynth sonic-pi-stereo_warp_sample [buf 0
                                         amp 1
                                         amp_slide 0
                                         amp_slide_shape 5
                                         amp_slide_curve 0
                                         pan 0
                                         pan_slide 0
                                         pan_slide_shape 5
                                         pan_slide_curve 0
                                         start 0
                                         finish 1
                                         rate 1
                                         rate_slide 0
                                         rate_slide_shape 5
                                         rate_slide_curve 0
                                         window_size 0.1
                                         overlaps 8
                                         out_bus 0]
    (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
          pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
          rate          (varlag rate rate_slide rate_slide_curve rate_slide_shape)
          play-time     (* (buf-dur buf) (absdif finish start))
          snd           (warp1:ar 2 buf (line start finish play-time) rate window_size overlaps 0 4)
          [snd-l snd-r] snd
          snd           (balance2 snd-l snd-r pan amp)
          ]
      (out out_bus snd)))







  )
