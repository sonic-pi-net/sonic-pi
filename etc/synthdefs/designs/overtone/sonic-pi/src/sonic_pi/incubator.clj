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


(ns sonic-pi.incubator
  (:use [overtone.live])
  (:require [sonic-pi.core :as core]))

;; Triggered synths
(do
  (without-namespace-in-synthdef

 (defsynth sonic-pi-fx_krush
   [amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 1
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 1
    pre_amp_slide_curve 0
    gain 4
    gain_slide 0
    gain_slide_shape 1
    gain_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 1
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix         (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp     (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         gain        (varlag gain gain_slide gain_slide_curve gain_slide_shape)
         cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
         res         (varlag res res_slide res_slide_curve res_slide_shape)
         cutoff-freq (midicps cutoff)

         [in-l in-r] (abs (* pre_amp (in in_bus 2)))
         new-l-sqr   (* in-l in-l)
         new-l       (/ (+ new-l-sqr (* gain in-l)) (+ new-l-sqr (* in-l (- gain 1)) 1))
         new-r-sqr   (* in-r in-r)
         new-r       (/ (+ new-r-sqr (* gain in-r)) (+ new-r-sqr (* in-r (- gain 1)) 1))
         new-l       (rlpf new-l cutoff-freq res)
         new-r       (rlpf new-r cutoff-freq res)
         fin-l       (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r       (x-fade2 in-r new-r (- (* mix 2) 1) amp)]

     (out out_bus [fin-l fin-r])))
   ;; your synths here
   )

  (save-to-pi sonic-pi-fx_krush)
  )

;;FX
(without-namespace-in-synthdef
 ;; BPF designs

 (defsynth sonic-pi-fx_reverb2
   [amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    mix 0.4
    mix_slide 0
    mix_slide_shape 1
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 1
    pre_amp_slide_curve 0
    rev_time 0.5
    rev_time_slide 0
    rev_time_slide_shape 1
    room 0.6
    max_room 1
    damp 0.5
    damp_slide 0
    damp_slide_shape 1
    damp_slide_curve 0
    in_damp 0.5
    in_damp_slide 0
    in_damp_slide_shape 1
    in_damp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp      (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix      (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp  (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         room     (* room 300)
         max_room (* max_room 300)
         damp     (varlag damp damp_slide damp_slide_curve damp_slide_shape)
         [l r]    (* pre_amp (in:ar in_bus 2))
         snd      (* amp (g-verb l r mix room damp))]
     (out out_bus snd)))


 (defsynth sonic-pi-fx_chorus
   [amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    mix 1
    mix_slide 0
    mix_slide_shape 1
    mix_slide_curve 0
    pre_amp 1
    pre_amp_slide 0
    pre_amp_slide_shape 1
    pre_amp_slide_curve 0
    phase 0.5
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    decay 0
    decay_slide 0
    decay_slide_shape 1
    decay_slide_curve 0
    max_phase 1
    amp 1
    amp_slide 0
    amp_slide_shape 1
    amp_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
         decay         (varlag decay decay_slide decay_slide_curve decay_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         ;; The phase here is the distance between two slow sin-oscs
         [new-l new-r] (+ [in-l in-r] (comb-n [in-l in-r] max_phase [(sin-osc:ar 0.0004) (sin-osc:ar (* 0.0001 (+ phase 0.4)))] decay))
         fin-l         (x-fade2 in-l new-l (- (* mix 2) 1) amp)
         fin-r         (x-fade2 in-r new-r (- (* mix 2) 1) amp)]
     (out out_bus [fin-l fin-r])))



 ;; END BPF designs

 ;; (def ab (audio-bus 2))
 ;; (def g (group :after (foundation-default-group)))
 ;; (sonic-pi-fx_chorus [:head g] :in_bus ab)

 ;; (run (out ab (pan2 (saw))))

 ;; (kill sonic-pi-fx_rbpf)

 (do ;;comment
   (save-to-pi sonic-pi-fx_chorus)
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
                                         amp_slide_shape 1
                                         amp_slide_curve 0
                                         pan 0
                                         pan_slide 0
                                         pan_slide_shape 1
                                         pan_slide_curve 0
                                         start 0
                                         finish 1
                                         rate 1
                                         rate_slide 0
                                         rate_slide_shape 1
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


  (without-namespace-in-synthdef
   (defsynth sonic-pi-singer
     "TODO: support vowel changes"
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

      attack 0
      decay 0
      sustain 0
      release 4
      attack_level 1
      sustain_level 1
      env_curve 1

      cutoff 110
      cutoff_slide 0
      cutoff_slide_shape 1
      cutoff_slide_curve 0
      res 0.3
      res_slide 0
      res_slide_shape 1
      res_slide_curve 0

      vibrato_speed 6
      vibrato_depth 4

      freq0 400
      freq1 750
      freq2 2400
      freq3 2600
      freq4 2900
      amp0 1
      amp1 0.28
      amp2 0.08
      amp3 0.1
      amp4 0.01
      qs0 0.1
      qs1 0.1
      qs2 0.04
      qs3 0.04
      qs4 0.04
      lag-val 0.5

      out_bus 0
      ]
     (let [pan         (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           note        (varlag note note_slide note_slide_curve note_slide_shape)
           amp         (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           cutoff      (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
           res         (varlag res res_slide res_slide_curve res_slide_shape)
           freq        (midicps note)

           freqs-list  (map #(lag:kr % lag-val) [freq0 freq1 freq2 freq3 freq4])
           amps-list   (map #(lag:kr (dbamp %) lag-val) [amp0 amp1 amp2 amp3 amp4])
           qs-list     (map #(lag:kr % lag-val) [qs0 qs1 qs2 qs3 qs4])
           cutoff-freq (midicps cutoff)
           vibrato     (* vibrato_depth (sin-osc:kr vibrato_speed))
           in          (saw:ar (lag:kr (+ freq vibrato) 0.2))

           env         (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level sustain_level env_curve) :action FREE)
           snd         (mix (* amps-list (bpf:ar in freqs-list qs-list)))
           snd         (rlpf snd cutoff-freq res)
           snd         (* snd amp)]
       (out out_bus (* (pan2 snd pan) env)))))

  (defsynth sonic-pi-dark_sea_horn
     "Dark, rough and sharp sea horn.
     Note: we are purposely not using recusion using busses. Just does not have the same feel."
     [out_bus 0

      note 52
      note_slide 0
      note_slide_shape 1
      note_slide_curve 0

      pan 0
      pan_slide 0
      pan_slide_shape 1
      pan_slide_curve 0

      amp 1
      amp_slide 0
      amp_slide_shape 1
      amp_slide_curve 0

      attack 1
      decay 0
      sustain 0
      release 4.0
      attack_level 1
      sustain_level 1
      env_curve 1]
     (let [note (varlag note note_slide note_slide_curve note_slide_shape)
           amp (varlag amp amp_slide amp_slide_curve amp_slide_shape)
           pan  (varlag pan pan_slide pan_slide_curve pan_slide_shape)
           freq (midicps note)

           a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* (lf-noise1:ar 0.1) 3))))

           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (tanh a)

           a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (tanh a)

           a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (tanh a)

           a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (tanh a)

           a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
           a (tanh a)

           env (env-gen:kr (core/shaped-adsr attack decay sustain release attack_level sustain_level env_curve) :action FREE)
           snd (* amp a)]
       (out out_bus (* env (pan2 snd pan)))))

  (comment
    (sonic-pi-dark_sea_horn :attack 1 :release 8 :note 40)
    (save-to-pi sonic-pi-dark_sea_horn)
    )

  (comment
    (defn bass          [] (singer :freq 100))
    (defn tenor         [] (singer :freq 280))
    (defn alto          [] (singer :freq 380))
    (defn soprano       [] (singer :freq 580))
    (def v (bass))

    (save-to-pi sonic-pi-singer)
    )

  )
