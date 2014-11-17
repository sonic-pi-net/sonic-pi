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
  (save-synthdef sdef "/Users/xavierriley/Projects/sonic-pi/etc/synthdefs"))


;; Triggered synths
(do
  (without-namespace-in-synthdef

    ;; your synths here
    )

  (comment
    ;; (save-to-pi sonic-pi-beep)
    ))

;;FX
(without-namespace-in-synthdef
 ;; BPF designs
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
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
         freq          (midicps freq)
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
    freq 100
    freq_slide 0
    freq_slide_shape 5
    freq_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
         freq          (midicps freq)
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
    freq 100
    freq_slide 0
    freq_slide_shape 5
    freq_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 5
    res_slide_curve 0
    in_bus 0
    out_bus 0]
   (let [amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
         mix           (varlag mix mix_slide mix_slide_curve mix_slide_shape)
         pre_amp       (varlag pre_amp pre_amp_slide pre_amp_slide_curve pre_amp_slide_shape)
         freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
         freq          (midicps freq)
         res           (varlag res res_slide res_slide_curve res_slide_shape)

         [in-l in-r]   (* pre_amp (in in_bus 2))
         [new-l new-r] (normalizer (resonz [in-l in-r] freq res))
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

 (defsynth sonic-pi-fx_chorus
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
    decay 0
    decay_slide 0
    decay_slide_shape 5
    decay_slide_curve 0
    max_phase 1
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
         ;; The phase here is the distance between two slow sin-oscs
         [new-l new-r] (+ [in-l in-r] (comb-n [in-l in-r] max_phase [(sin-osc:ar 0.0004) (sin-osc:ar (* 0.0001 (+ phase 0.4)))] decay))
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

 ;; END BPF designs

 ;; (def ab (audio-bus 2))
 ;; (def g (group :after (foundation-default-group)))
 ;; (sonic-pi-fx_chorus [:head g] :in_bus ab)

 ;; (run (out ab (pan2 (saw))))

 ;; (kill sonic-pi-fx_rbpf)

 (do ;;comment
   (save-to-pi sonic-pi-fx_bpf)
   (save-to-pi sonic-pi-fx_rbpf)
   (save-to-pi sonic-pi-fx_nrbpf)
   (save-to-pi sonic-pi-fx_ring_mod)
   (save-to-pi sonic-pi-fx_chorus)
   (save-to-pi sonic-pi-fx_octaver)
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
