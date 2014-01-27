(ns sp
  (:use [overtone.live])
  (:require [clojure.string :as str]))

(defn save-synthdef [sdef folder]
  (let [path (str folder "/" (last (str/split (-> sdef :sdef :name) #"/")) ".scsyndef") ]
    (overtone.sc.machinery.synthdef/synthdef-write (:sdef sdef) path)
    path))

(defn save-to-pi [sdef]
  (save-synthdef sdef "/Users/sam/Dropbox/lab/code/defining-pi/etc/synthdefs/"))

(do
  (defsynth space_organ [note 24 amp 1 x 0 y 0 out-bus 0]
    (let [freq-shift (/ x 100)
          delay (* -1 (/ x 10000))]
      (out out-bus (pan2  (g-verb (* 0.2 (mix (map #(blip (+ freq-shift (* (midicps (duty:kr % 0 (dseq [note (+ 3 note) (+ 7 note) (+ 12 note) (+ 17 note)] INF))) %2)) (mul-add:kr (lf-noise1:kr 1/2) 3 4)) (+ delay [1 1/4]) [1  8]))) 200 8)))))


  (save-to-pi space_organ)

  (defsynth saws [note 52 x 0 y 0 out-bus 0]
    (let [x    (abs x)
          x    (min x 10000)
          x    (max x 50)
          y    (abs y)
          y    (/ y 10000)
          y    (min y 0.3)
          y    (max y 0)
          freq (midicps note)]
      (out out-bus (mix (* 0.15 (normalizer (lpf (saw [freq (+ freq (* freq y))]) x)))))))

  (save-to-pi saws))


(do
  (defsynth trig))

(do
  (defsynth beep [note 52 attack 0.1 release 0.3 out-bus 0]
    (let [freq (midicps note)]
      (out out-bus (* (sin-osc freq)
                      (env-gen (perc attack release) :action FREE)))))

  (save-to-pi beep))

(do
  (defsynth saw_beep [note 52 attack 0.1 release 0.3 out-bus 0]
    (let [freq (midicps note)]
      (out out-bus (* 0.3
                      (saw [freq (+ freq (* freq 0.01))])
                      (env-gen (perc attack release) :action FREE)))))

  (save-to-pi saw_beep))

(do
  (defsynth loop-synth [buf 0 vol 1 rate 1 out-bus 0]
    (let [src (play-buf 1 buf rate 1.0 0.0 1.0 1)]
      (out out-bus (pan2 (* src vol)))))
  (save-to-pi loop-synth))

(do
  (defsynth mixer [in-bus 0 amp 1 pan 0]
    (let [src (in:ar in-bus 1)
          src (lpf src 20000)]
      (out 0 (pan2 (* (lag-ud amp 0 0.02) src) pan))))

  (save-to-pi mixer))

(do
  ;;TODO FIXME!
  (defsynth babbling [out-bus 0 x 0 y 50]
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
      (out out-bus (* 0 (mix (* 3 mixed))))))
  (save-to-pi babbling))

(do
  (defsynth fm [note 52 divisor 2.0 depth 1.0 out-bus 0]
    (let [carrier   (midicps note)
          modulator (/ carrier divisor)
          mod-env   (env-gen (lin-env 1 0 1))
          amp-env   (env-gen (lin-env 0 1 1) :action FREE)]
      (out out-bus (* 0.25 (* amp-env
                             (sin-osc (+ carrier
                                         (* mod-env  (* carrier depth) (sin-osc modulator)))))))))

  (save-to-pi fm))

(do

  (save-to-pi stereo-player)
  (save-to-pi mono-player)
 )


(do
  (defsynth woah [note 52 out-bus 0 x 0 y 0]
    (let [freq (midicps note)
          x    (abs x)
          x    (/ x 700)
          x    (min x 15)
          x    (max x 0.5)
          snd  (lpf (sync-saw
                     freq
                     (* (* freq 1.5) (+ 2 (sin-osc:kr x))))
                    1000)]
      (out out-bus (* 0.25 snd))))

  (save-to-pi woah))


(do
  (defsynth arpeg-click [x 10 buf 0 arp-div 2 beat-div 1 out-bus 0]
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
      (out out-bus (* 0.20 (pan2 (+ (* 0.5 snd b-env)
                                    (* (sin-osc freq) a-env)
                                    (* (sin-osc (* 2 freq)) a-env)))))))

  (save-to-pi arpeg-click))

(do
  ;;http://computermusicresource.com/Simple.bell.tutorial.html
(def dull-partials
  [
   0.56
;;   1.19
;;   1.71
;;   2.74
   3
;;   3.76
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
;;   6.8
   ])

;; we make a bell by combining a set of sine waves at the given
;; proportions of the frequency. Technically not really partials
;; as for the 'pretty bell' I stuck mainly with harmonics.
;; Each partial is mixed down proportional to its number - so 1 is
;; louder than 6. Higher partials are also supposed to attenuate
;; quicker but setting the release didn't appear to do much.

(defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   attack {:default 0.01}
   release  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (env-gen (perc attack (* release proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))


(defsynth dull_bell [note 52 attack 0.01 release 1.0 vol 1.0 out-bus 0]
  (let [freq (midicps note)
        snd (* vol (bell-partials freq attack release dull-partials))]
    (detect-silence snd :action FREE)
    (out out-bus snd)))

(defsynth pretty_bell [note 52 attack 0.01 release 1 vol 1.0 out-bus 0]
  (let [freq (midicps note)
        snd (* vol (bell-partials freq attack release partials))]
    (detect-silence snd :action FREE)
    (out out-bus snd)))

(save-to-pi dull_bell)
(save-to-pi pretty_bell))


;; SynthDef(\babblingbrook_jmc, { |out=0, amp=0.1|
;;      var son;

;;      son = ({RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 14)
;;              * 400 + 500, 0.03, 0.003)}!2)
;;              + ({RHPF.ar(OnePole.ar(BrownNoise.ar, 0.99), LPF.ar(BrownNoise.ar, 20)
;;              * 800 + 1000, 0.03, 0.005)}!2)  * 4;
;;      Out.ar(out, son * (amp * 20))
;; },
