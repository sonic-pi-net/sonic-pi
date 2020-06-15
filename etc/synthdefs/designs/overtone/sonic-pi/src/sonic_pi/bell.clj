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


(ns sonic-pi.bell
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))


(without-namespace-in-synthdef
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
     decay-level {:default 1}
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
                                 note_slide_shape 1
                                 note_slide_curve 0
                                 amp 1
                                 amp_slide 0
                                 ;;weirdly, having a shape of 2 craps out scsynth!
                                 amp_slide_shape 1
                                 amp_slide_curve 0
                                 pan 0
                                 pan_slide 0
                                 pan_slide_shape 1
                                 pan_slide_curve 0
                                 attack 0
                                 decay 0
                                 sustain 0
                                 release 1.0
                                 attack_level 1
                                 decay_level -1
                                 sustain_level 1
                                 env_curve 1
                                 out_bus 0]
     (let [decay_level (select:kr (= -1 decay_level) [decay_level sustain_level])
           note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq        (midicps note)
           snd         (* amp (bell-partials freq attack decay sustain release attack_level decay_level sustain_level dull-partials))]
       (detect-silence snd :action FREE)
       (out out_bus (pan2 snd pan))))

   (defsynth sonic-pi-pretty_bell [note 52
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
           pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq        (midicps note)
           snd         (* amp (bell-partials freq attack decay sustain release attack_level decay_level sustain_level partials))]
       (detect-silence snd :action FREE)
       (out out_bus (pan2 snd pan)))))

  (comment
    (core/save-synthdef sonic-pi-dull_bell)
    (core/save-synthdef sonic-pi-pretty_bell)
))
