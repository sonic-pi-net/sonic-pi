(ns sp
  (:use [overtone.live])
  (:require [clojure.string :as str]))

;; Utility functions (for creating and storing synthdefs)

(defn save-synthdef [sdef folder]
  (let [path (str folder "/" (last (str/split (-> sdef :sdef :name) #"/")) ".scsyndef") ]
    (overtone.sc.machinery.synthdef/synthdef-write (:sdef sdef) path)
    path))

(defn save-to-pi [sdef]
  (save-synthdef sdef "/Users/sam/Development/RPi/sonic-pi/etc/synthdefs/"))


;; Main mixer



(do

  (defsynth mixer [in-bus 0 amp 1 safe-recovery-time 3]
    (let [source   (in in-bus 2)
          source   (* amp source)
          source   (lpf source 20000)
          amp      (lag-ud amp 0 0.02)
          safe-snd (limiter source 0.99 0.001)]
        (replace-out 0 safe-snd)))

  ;; (save-to-pi mixer)
  )


;; Simple Trigger synths

(do
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
    [freq     {:default 440 :doc "The fundamental frequency for the partials"}
     attack   {:default 0.01}
     sustain  {:default 0}
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
               (let [vol      (/ proportion 2)
                     env      (env-gen (envelope [0 1 1 0] [attack sustain (* release proportion)]) :level-scale vol)
                     overtone (* partial freq)]
                 (* env (sin-osc overtone))))
             partials ;; current partial
             (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
             ))))


  (defsynth dull_bell [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 1.0 slide 0 out-bus 0]
    (let [note (lag note slide)
          freq (midicps note)
          snd  (* amp (bell-partials freq attack sustain release dull-partials))]
      (detect-silence snd :action FREE)
      (out out-bus (pan2 snd pan))))

  (defsynth pretty_bell [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 1 slide 0 out-bus 0]
    (let [note (lag note slide)
          freq (midicps note)
          snd  (* amp (bell-partials freq attack sustain release partials))]
      (detect-silence snd :action FREE)
      (out out-bus (pan2 snd pan))))

  (defsynth beep [note 52 amp 1 pan 0 attack 0.1 sustain 0 release 0.3 slide 0 out-bus 0]
    (let [note (lag note slide)
          freq (midicps note)]
      (out out-bus (pan2 (* (sin-osc freq)
                            (env-gen (envelope [0 1 1 0] [attack sustain release]) :level-scale amp :action FREE)
)
                         pan))))

  (defsynth saw_beep [note 52 amp 1 pan 0 attack 0.1 sustain 0 release 0.3 slide 0 cutoff 100 cutoff_slide 0 out-bus 0]
    (let [note        (lag note slide)
          cutoff      (lag cutoff cutoff_slide)
          freq        (midicps note)
          cutoff-freq (midicps cutoff)]
      (out out-bus (pan2 (* (normalizer (lpf (saw freq) cutoff-freq))
                            (env-gen (envelope [0 1 1 0] [attack sustain release]) :level-scale amp :action FREE))
                         pan))))

  (defsynth dsaw [note 52 amp 1 pan 0 detune 0.1 attack 0.1 sustain 0 release 0.3 slide 0 cutoff 100 cutoff_slide 0 out-bus 0]
    (let [note        (lag note slide)
          cutoff      (lag cutoff cutoff_slide)
          freq        (midicps note)
          cutoff-freq (midicps cutoff)
          detune-freq (midicps (+ note detune))]
      (out out-bus (pan2 (* (normalizer (lpf (mix (saw [freq detune-freq])) cutoff-freq))
                            (env-gen (envelope [0 1 1 0] [attack sustain release]) :level-scale amp :action FREE)
)
                         pan))))

  (defsynth fm [note 52 amp 1 pan 0 attack 1 sustain 0 release 1 slide 0 divisor 2.0 depth 1.0 mod_slide 0 out-bus 0]
    (let [note      (lag note slide)
          carrier   (midicps note)
          modulator (/ carrier divisor)
          modulator (lag modulator mod_slide)
          env       (env-gen (env-lin attack sustain release) :level-scale amp :action FREE)]
      (out out-bus (pan2 (* env
                            (sin-osc (+ carrier
                                        (* env  (* carrier depth) (sin-osc modulator)))))
                         pan))))


  (defsynth mod_saw [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 100 cutoff_slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          cutoff         (lag cutoff cutoff_slide)
          freq           (midicps note)
          cutoff-freq    (midicps cutoff)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (saw freq)
          snd            (lpf snd cutoff-freq)
          snd            (normalizer snd)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_saw_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          freq           (midicps note)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (saw freq)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_dsaw [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 100 cutoff_slide 0 mod_rate 1 mod_range 5 mod_width 0.5 detune 0.1 out-bus 0]
    (let [note           (lag note slide)
          cutoff         (lag cutoff cutoff_slide)
          freq           (midicps note)
          cutoff-freq    (midicps cutoff)
          mod-range-freq (- (midicps (+ mod_range note))
                            freq)
          detune-freq    (midicps (+ note detune))
          freq-mod       (* mod-range-freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (mix (saw [freq detune-freq]))
          snd            (lpf snd cutoff-freq)
          snd            (normalizer snd)
          env            (env-gen (env-lin attack release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_dsaw_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 mod_rate 1 mod_range 5 mod_width 0.5 detune 0.1 out-bus 0]
    (let [note           (lag note slide)
          freq           (midicps note)
          mod-range-freq (- (midicps (+ mod_range note))
                            freq)
          detune-freq    (midicps (+ note detune))
          freq-mod       (* mod-range-freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (mix (saw [freq detune-freq]))
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_sine [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 100 cutoff_slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          cutoff         (lag cutoff cutoff_slide)
          freq           (midicps note)
          cutoff-freq    (midicps cutoff)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (sin-osc freq)
          snd            (lpf snd cutoff-freq)
          snd            (normalizer snd)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_sine_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          freq           (midicps note)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (sin-osc freq)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_tri [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 100 cutoff_slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          cutoff         (lag cutoff cutoff_slide)
          freq           (midicps note)
          cutoff-freq    (midicps cutoff)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (lf-tri freq)
          snd            (lpf snd cutoff-freq)
          snd            (normalizer snd)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_tri_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 mod_rate 1 mod_range 5 mod_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          freq           (midicps note)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (lf-tri freq)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_pulse [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 100 cutoff_slide 0 mod_rate 1 mod_range 5 mod_width 0.5 pulse_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          cutoff         (lag cutoff cutoff_slide)
          freq           (midicps note)
          cutoff-freq    (midicps cutoff)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (pulse freq pulse_width)
          snd            (lpf snd cutoff-freq)
          snd            (normalizer snd)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))

  (defsynth mod_pulse_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 mod_rate 1 mod_range 5 mod_width 0.5 pulse_width 0.5 out-bus 0]
    (let [note           (lag note slide)
          freq           (midicps note)
          mod_range_freq (- (midicps (+ mod_range note))
                            freq)
          freq-mod       (* mod_range_freq (lf-pulse mod_rate 0.5 mod_width))
          freq           (+ freq freq-mod)
          snd            (pulse freq pulse_width)
          env            (env-gen (env-lin attack sustain release) :action FREE)]
      (out out-bus (pan2 (* env snd) pan amp))))


  ;; (save-to-pi dull_bell)
  ;; (save-to-pi pretty_bell)
  ;; (save-to-pi beep)
  ;; (save-to-pi saw_beep)
  ;; (save-to-pi dsaw)
  ;; (save-to-pi fm)

  ;; (save-to-pi mod_saw)
  ;; (save-to-pi mod_saw_s)
  ;; (save-to-pi mod_dsaw)
  ;; (save-to-pi mod_dsaw_s)
  ;; (save-to-pi mod_sine)
  ;; (save-to-pi mod_sine_s)
  ;; (save-to-pi mod_tri)
  ;; (save-to-pi mod_tri_s)
  ;; (save-to-pi mod_pulse)
  ;; (save-to-pi mod_pulse_s)
  )


;; Sample playback synths

(do

  (defsynth basic-mono-player
    [buf 0 amp 1 pan 0 rate 1 out-bus 0]
    (let [rate (* rate (buf-rate-scale buf))
          snd  (play-buf 1 buf rate)]
      (out out-bus (pan2 snd pan  amp))))

  (defsynth basic-stereo-player
    [buf 0 amp 1 pan 0 rate 1 out-bus 0]
    (let [rate (* rate (buf-rate-scale buf))
          snd  (play-buf 2 buf rate)]
      (out out-bus (pan2 snd pan amp))))

  (defsynth mono-player
    "Plays a mono buffer from start pos to end pos (represented as
     values between 0 and 1). Outputs a stereo signal."
    [buf 0 amp 1 pan 0 attack 0.0 sustain -1 release 0.0 rate 1 start 0 end 1 out-bus 0 ]
    (let [n-frames    (- (buf-frames buf) 1)
          start-pos   (* start n-frames)
          end-pos     (* end n-frames)
          n-start-pos (select:kr (not-pos? rate) [start-pos end-pos])
          n-end-pos   (select:kr (not-pos? rate) [end-pos start-pos])
          rate        (abs rate)
          play-time   (/ (* (buf-dur buf) (absdif end start))
                         rate)
          phase       (line:ar :start n-start-pos :end n-end-pos :dur play-time :action FREE)
          sustain     (select:kr (= -1 sustain) [sustain (- play-time attack release)])
          env         (env-gen (envelope [0 1 1 0] [attack sustain release]) :action FREE)
          snd         (buf-rd 1 buf phase)
          snd         (* env snd)
          snd         (pan2 snd pan amp)]
      (out out-bus snd)))

  (defsynth stereo-player
    "Plays a mono buffer from start pos to end pos (represented as
     values between 0 and 1). Outputs a stereo signal."
    [buf 0 amp 1 pan 0 attack 0.0 sustain -1 release 0.0 rate 1 start 0 end 1 out-bus 0 ]
    (let [n-frames      (- (buf-frames buf) 1)
          start-pos     (* start n-frames)
          end-pos       (* end n-frames)
          n-start-pos   (select:kr (not-pos? rate) [start-pos end-pos])
          n-end-pos     (select:kr (not-pos? rate) [end-pos start-pos])
          rate          (abs rate)
          play-time     (/ (* (buf-dur buf) (absdif end start))
                           rate)
          phase         (line:ar :start n-start-pos :end n-end-pos :dur play-time :action FREE)
          sustain       (select:kr (= -1 sustain) [sustain (- play-time attack release)])
          env           (env-gen (envelope [0 1 1 0] [attack sustain release]) :action FREE)
          [snd-l snd-r] (buf-rd 2 buf phase)
          snd           (balance2 snd-l snd-r pan amp)
          snd           (* env snd)]
      (out out-bus snd)))

  ;; (save-to-pi mono-player)
  ;; (save-to-pi stereo-player)
  ;; (save-to-pi basic-mono-player)
  ;; (save-to-pi basic-stereo-player)

  )

(do

  (defsynth tb303
  "A simple clone of the sound of a Roland TB-303 bass synthesizer."
  [note     52        ; midi note value input
   amp      1
   pan      0
   attack   0.01
   sustain  0
   release  2
   slide    0
   cutoff   80
   cutoff_min 30
   res      0.2       ; rlpf resonance
   wave     0         ; 0=saw, 1=pulse
   pulse_width 0.5    ; only for pulse wave
   out-bus  0]
  (let [note            (lag note slide)
        cutoff-freq     (midicps cutoff)
        cutoff-min-freq (midicps cutoff_min)
        freq            (midicps note)
        env             (env-gen (env-lin attack sustain release) :action FREE)
        snd             (rlpf (select wave [(saw freq) (pulse freq pulse_width)])
                              (+ cutoff-min-freq (* env cutoff-freq))
                              res)
        snd             (* env snd)]
    (out out-bus (pan2 snd pan amp))))

  (defsynth supersaw [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 130 cutoff_slide 0 res 0.3 out-bus 0]
    (let [note        (lag note slide)
          cutoff      (lag cutoff cutoff_slide)
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
          env         (env-gen (env-lin attack sustain release) :action FREE)
          output      (* env output)
          output      (pan2 output pan amp)]
      (out out-bus output)))

  (defsynth supersaw_s [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 out-bus 0]
    (let [note        (lag note slide)
          freq        (midicps note)
          input       (lf-saw freq)
          shift1      (lf-saw 4)
          shift2      (lf-saw 7)
          shift3      (lf-saw 2)
          comp1       (> input shift1)
          comp2       (> input shift2)
          comp3       (> input shift3)
          output      (+ (- input comp1) (- input comp2) (- input comp3))
          output      (- output input)
          output      (leak-dc:ar (* output 0.333))
          env         (env-gen (env-lin attack sustain release) :action FREE)
          output      (* env output)
          output      (pan2 output pan amp)]
      (out out-bus output)))

  (defsynth prophet
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

  [note 52 amp 1 pan 0 attack 0.01 sustain 0 release 2 slide 0 cutoff 110 cutoff_slide 0 res 0.3 out-bus 0 ]

  (let [note        (lag note slide)
        cutoff      (lag cutoff cutoff_slide)
        freq        (midicps note)
        cutoff-freq (midicps cutoff)
        snd         (mix [(pulse freq (* 0.1 (/ (+ 1.2 (sin-osc:kr 1)) )))
                          (pulse freq (* 0.8 (/ (+ 1.2 (sin-osc:kr 0.3) 0.7) 2)))
                          (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 )) 2)))
                          (pulse freq (* 0.8 (/ (+ 1.2 (lf-tri:kr 0.4 0.19)) 2)))
                          (* 0.5 (pulse (/ freq 2) (* 0.8 (/ (+ 1.2 (lf-tri:kr (+ 2 (lf-noise2:kr 0.2))))
                                                             2))))])
        snd         (normalizer snd)
        env         (env-gen (env-lin attack sustain release) :action FREE)
        snd         (rlpf (* env snd snd) cutoff-freq res)
        snd         (pan2 snd pan amp)]

    (out out-bus snd)))

  ;;(save-to-pi tb303)
  ;;(save-to-pi supersaw)
  ;;(save-to-pi supersaw_s)
  ;;(save-to-pi prophet)
  )

(do)


;; Experimental
(comment
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

    (save-to-pi arpeg-click) )

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

    (save-to-pi saws) )

  (mod_dsaw 52))
