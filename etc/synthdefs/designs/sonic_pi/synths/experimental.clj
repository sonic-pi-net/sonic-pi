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

(ns sonic-pi.synths.experimental
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))

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







  )

(without-namespace-in-synthdef
 (core/def-fx sonic-pi-fx_guitar_saw
   []
   [
    amp (* 1 (lag (amplitude dry-r)))

    dry-r (*  1 (compander dry-r dry-r
                            0.2  ;; thresh
                            10    ;; slope below
                            1    ;; slope above
                            0.01 ;; clamp-time
                            0.01))

    dry-r (*  1 (compander dry-r dry-r
                            0.2  ;; thresh
                            1    ;; slope below
                            0.5    ;; slope above
                            0.01 ;; clamp-time
                            0.01)) ;; relax-time
    dry-r (lpf dry-r 800)
    dry-r (hpf dry-r 80)

    freq (pitch dry-r
                100  ;; :init-freq 100
                60   ;; :min-freq 80
                1300 ;; :max-freq 1200
                400  ;; :exec-freq 100
                32   ;; :max-bins-per-octave 32
                1    ;; :median 1
                0.01 ;; :amp-threshold 0.01
                0.1 ;; :peak_threshold 0.01
                0    ;; :down-sample 1
                7) ;; :clar 0


    freq (lag freq 0.01)

    snd (* 1 amp (+ (saw freq)
                    (+ 5 (saw freq))
                    (* 2 (saw (/ freq 2)))))


    ;; snd (* 1 amp (+ (lf-tri freq)
    ;;                 (+ 5 (lf-tri freq))
    ;;                 (* 2 (lf-tri (/ freq 2)))))


    snd (* 1 amp (+ (sin-osc freq)
                    (sin-osc (* 4 freq))
                    (* 4 (sin-osc (/ freq 2)))
                    ))
    [wet-l wet-r] (pan2 snd)])

 (core/save-synthdef sonic-pi-fx_guitar_saw)

 )

(without-namespace-in-synthdef
 (core/def-fx sonic-pi-fx_guitar_saw2
   [octave 0
    default_note 48]
   [
    amp (* 1 (lag (amplitude dry-r)))

    ;; dry-r (*  1 (compander dry-r dry-r
    ;;                         0.2  ;; thresh
    ;;                         10    ;; slope below
    ;;                         1    ;; slope above
    ;;                         0.01 ;; clamp-time
    ;;                         0.01))

    dry-r (*  1 (compander dry-r dry-r
                            0.2  ;; thresh
                            1    ;; slope below
                            0.5    ;; slope above
                            0.01 ;; clamp-time
                            0.01)) ;; relax-time

    min-freq  20
    max-freq 400
    ;; guitar values
    ;; dry-r (lpf dry-r 1200)
    ;; dry-r (hpf dry-r 80)

    ;; bass values
    dry-r (lpf dry-r max-freq)
    dry-r (hpf dry-r min-freq)
    init-freq (midicps default_note)
    freq (pitch dry-r
                73.416  ;; :init-freq 100
                20   ;; :min-freq 80
                400         ;; :max-freq 1200
                100  ;; :exec-freq 100
                32  ;; :max-bins-per-octave 32
                1    ;; :median 1
                0.01 ;; :amp-threshold 0.01
                0.1 ;; :peak_threshold 0.01
                1   ;; :down-sample 1
                7)
    freq (lag freq 0.05)
    freq-nd (send-reply (impulse:kr 4) "/scsynth/freq" [freq])
;    freq (lag (* (+ octave  1) (/ freq 4)) 0.01)


    saws (* 1 amp (+ (saw freq) (+ 5 (saw freq))
                    (* 2 (saw (/ freq 2)))))


    tris (* 1 amp (+ (lf-tri freq)
                    (+ 5 (lf-tri freq))
                    (* 2 (lf-tri (/ freq 2)))))

    sqrs (* 1 amp (+ (pulse freq)
                    (+ 5 (pulse freq))
                    (* 2 (pulse (/ freq 2)))))


    sins (* 1 amp (+ (sin-osc freq)
;;                    (sin-osc (* 4 freq))
;;                    (* 4 (sin-osc (/ freq 2)))
                     ))



    snd sqrs

    [wet-l wet-r] (pan2 snd)])

 (core/save-synthdef sonic-pi-fx_guitar_saw)

)
