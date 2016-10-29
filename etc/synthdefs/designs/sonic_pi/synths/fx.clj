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

(ns sonic-pi.synths.fx
  (:use [overtone.live])
  (:require [sonic-pi.synths.core :as core]))


(without-namespace-in-synthdef

 (core/def-fx sonic-pi-fx_mono
   [pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0
    ]
   [
    pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
    pan           (clip2 pan 1)
    mono          (/ (sum [dry-l dry-r]) 2)
    [wet-l wet-r] (pan2 mono pan amp)])


 (core/def-fx sonic-pi-fx_vowel
   [voice 0
    vowel_sound 1]
   [freqs            [800, 1150, 2900, 3900, 4950
                      350, 2000, 2800, 3600, 4950
                      270, 2140, 2950, 3900, 4950
                      450, 800, 2830, 3800, 4950
                      325, 700, 2700, 3800, 4950
                      800, 1150, 2800, 3500, 4950
                      400, 1600, 2700, 3300, 4950
                      350, 1700, 2700, 3700, 4950
                      450, 800, 2830, 3500, 4950
                      325, 700, 2530, 3500, 4950
                      660, 1120, 2750, 3000, 3350
                      440, 1800, 2700, 3000, 3300
                      270, 1850, 2900, 3350, 3590
                      430, 820, 2700, 3000, 3300
                      370, 630, 2750, 3000, 3400
                      650, 1080, 2650, 2900, 3250
                      400, 1700, 2600, 3200, 3580
                      290, 1870, 2800, 3250, 3540
                      400, 800, 2600, 2800, 3000
                      350, 600, 2700, 2900, 3300
                      600, 1040, 2250, 2450, 2750
                      400, 1620, 2400, 2800, 3100
                      250, 1750, 2600, 3050, 3340
                      400, 750, 2400, 2600, 2900
                      350, 600, 2400, 2675, 2950]
    amps             [1.0 0.5011872336272722 0.025118864315095784 0.09999999999999998 0.0031622776601683764
                      1.0 0.09999999999999998 0.17782794100389226 0.009999999999999995 0.0015848931924611136
                      1.0 0.251188643150958 0.050118723362727206 0.050118723362727206 0.006309573444801925
                      1.0 0.2818382931264453 0.0794328234724281 0.0794328234724281 0.0031622776601683764
                      1.0 0.1584893192461113 0.017782794100389226 0.009999999999999995 9.999999999999994E-4
                      1.0 0.6309573444801931 0.09999999999999998 0.015848931924611127 9.999999999999994E-4
                      1.0 0.06309573444801932 0.031622776601683784 0.017782794100389226 9.999999999999994E-4
                      1.0 0.09999999999999998 0.031622776601683784 0.015848931924611127 9.999999999999994E-4
                      1.0 0.3548133892335754 0.1584893192461113 0.03981071705534973 0.001778279410038922
                      1.0 0.251188643150958 0.031622776601683784 0.009999999999999995 6.309573444801923E-4
                      1.0 0.5011872336272722 0.07079457843841379 0.06309573444801932 0.012589254117941666
                      1.0 0.19952623149688797 0.12589254117941667 0.09999999999999998 0.09999999999999998
                      1.0 0.06309573444801932 0.06309573444801932 0.015848931924611127 0.015848931924611127
                      1.0 0.3162277660168379 0.050118723362727206 0.0794328234724281 0.019952623149688792
                      1.0 0.09999999999999998 0.07079457843841379 0.031622776601683784 0.019952623149688792
                      1.0 0.5011872336272722 0.44668359215096315 0.3981071705534972 0.0794328234724281
                      1.0 0.19952623149688797 0.251188643150958 0.19952623149688797 0.09999999999999998
                      1.0 0.17782794100389226 0.12589254117941667 0.09999999999999998 0.031622776601683784
                      1.0 0.3162277660168379 0.251188643150958 0.251188643150958 0.050118723362727206
                      1.0 0.09999999999999998 0.14125375446227542 0.19952623149688797 0.050118723362727206
                      1.0 0.44668359215096315 0.3548133892335754 0.3548133892335754 0.09999999999999998
                      1.0 0.251188643150958 0.3548133892335754 0.251188643150958 0.12589254117941667
                      1.0 0.031622776601683784 0.1584893192461113 0.0794328234724281 0.03981071705534973
                      1.0 0.2818382931264453 0.08912509381337454 0.09999999999999998 0.009999999999999995
                      1.0 0.09999999999999998 0.025118864315095784 0.03981071705534973 0.015848931924611127]
    bws              [1/80 1/90 1/120 1/130 1/140
                      1/60 1/100 1/120 1/150 1/200
                      1/60 1/90 1/100 1/120 1/120
                      1/70 1/80 1/100 1/130 1/135
                      1/50 1/60 1/170 1/180 1/200
                      1/80 1/90 1/120 1/130 1/140
                      1/60 1/80 1/120 1/150 1/200
                      1/50 1/100 1/120 1/150 1/200
                      1/70 1/80 1/100 1/130 1/135
                      1/50 1/60 1/170 1/180 1/200
                      1/80 1/90 1/120 1/130 1/140
                      1/70 1/80 1/100 1/120 1/120
                      1/40 1/90 1/100 1/120 1/120
                      1/40 1/80 1/100 1/120 1/120
                      1/40 1/60 1/100 1/120 1/120
                      1/80 1/90 1/120 1/130 1/140
                      1/70 1/80 1/100 1/120 1/120
                      1/40 1/90 1/100 1/120 1/120
                      1/40 1/80 1/100 1/120 1/120
                      1/40 1/60 1/100 1/120 1/120
                      1/60 1/70 1/110 1/120 1/130
                      1/40 1/80 1/100 1/120 1/120
                      1/60 1/90 1/100 1/120 1/120
                      1/40 1/80 1/100 1/120 1/120
                      1/40 1/80 1/100 1/120 1/120]
    ;; voice is 0 indexed, vowel_sound is 1 indexed
    vowel_freq_one   (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 5)) freqs)
    vowel_freq_two   (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 4)) freqs)
    vowel_freq_three (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 3)) freqs)
    vowel_freq_four  (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 2)) freqs)
    vowel_freq_five  (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 1)) freqs)
    vowel_amp_one    (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 5)) amps)
    vowel_amp_two    (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 4)) amps)
    vowel_amp_three  (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 3)) amps)
    vowel_amp_four   (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 2)) amps)
    vowel_amp_five   (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 1)) amps)
    vowel_bw_one     (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 5)) bws)
    vowel_bw_two     (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 4)) bws)
    vowel_bw_three   (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 3)) bws)
    vowel_bw_four    (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 2)) bws)
    vowel_bw_five    (select:kr (+ (* 5 voice) (- (* 5 vowel_sound) 1)) bws)

    amp-fudge        5
    pre-l            (* amp-fudge dry-l)
    pre-r            (* amp-fudge dry-r)
    wet-l            (* amp-fudge
                        (+ (b-band-pass pre-l vowel_freq_one (* vowel_bw_one vowel_amp_one))
                           (b-band-pass pre-l vowel_freq_two (* vowel_bw_two vowel_amp_two))
                           (b-band-pass pre-l vowel_freq_three (* vowel_bw_three vowel_amp_three))
                           (b-band-pass pre-l vowel_freq_four (* vowel_bw_four vowel_amp_four))
                           (b-band-pass pre-l vowel_freq_five (* vowel_bw_five vowel_amp_five))))
    wet-r            (* amp-fudge
                        (+ (b-band-pass pre-r vowel_freq_one (* vowel_bw_one vowel_amp_one))
                           (b-band-pass pre-r vowel_freq_two (* vowel_bw_two vowel_amp_two))
                           (b-band-pass pre-r vowel_freq_three (* vowel_bw_three vowel_amp_three))
                           (b-band-pass pre-r vowel_freq_four (* vowel_bw_four vowel_amp_four))
                           (b-band-pass pre-r vowel_freq_five (* vowel_bw_five vowel_amp_five))))
    ])

 (core/def-fx sonic-pi-fx_krush
   [gain 5
    gain_slide 0
    gain_slide_shape 1
    gain_slide_curve 0
    cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    ]
   [gain          (varlag gain gain_slide gain_slide_curve gain_slide_shape)
    cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)
    cutoff-freq   (midicps cutoff)

    new-l-abs     (abs dry-l)
    new-l-sqr     (squared new-l-abs)
    new-l         (/ (+ new-l-sqr (* gain new-l-abs)) (+ new-l-sqr (* new-l-abs (- gain 1)) 1))

    new-r-abs     (abs dry-r)
    new-r-sqr     (squared new-r-abs)
    new-r         (/ (+ new-r-sqr (* gain new-r-abs)) (+ new-r-sqr (* new-r-abs (- gain 1)) 1))

    wet-l         (rlpf new-l cutoff-freq res)
    wet-r         (rlpf new-r cutoff-freq res)])



 (core/def-fx sonic-pi-fx_whammy
     [transpose 12
      transpose_slide 0
      transpose_slide_shape 1
      transpose_slide_curve 0
      deltime 0.05
      max_delay_time 1
      grainsize 0.075
      ]
     [transpose   (varlag (midiratio transpose) transpose_slide transpose_slide_curve transpose_slide_shape)
      grainfreq   (/ transpose grainsize)
      wet-l       (leak-dc (* (delay-c :in (* pre_amp dry-l)
                                       :max-delay-time max_delay_time
                                       :delay-time (+ (* (lf-saw grainfreq 1.0)
                                                         (/ grainsize -2))
                                                      deltime))
                              (lin-lin (sin-osc grainfreq
                                                (/ (* -1 Math/PI) 2))
                                       -1 1 0 1 )))
        wet-r       (leak-dc (* (delay-c :in (* pre_amp dry-r)
                                         :max-delay-time max_delay_time
                                         :delay-time (+ (* (lf-saw grainfreq 0.0)
                                                           (/ grainsize -2))
                                                        deltime))
                                (lin-lin (sin-osc grainfreq
                                                  (/ Math/PI 2))
                                         -1 1 0 1 )))])

 (core/def-fx sonic-pi-fx_tanh
   [
    krunch 5
    krunch_slide 0
    krunch_slide_shape 1
    krunch_slide_curve 0
    ]
   [krunch        (varlag krunch krunch_slide krunch_slide_curve krunch_slide_shape)
    krunch        (select:kr (= 0 krunch) [krunch 0.0001])
    krunch        (* 5 krunch)
    [wet-l wet-r] (* (/ (tanh (* krunch [dry-l dry-r])) krunch) (+ 1 (/ krunch 8)))])



 (core/def-fx sonic-pi-fx_bitcrusher
   [sample_rate 10000
    sample_rate_slide 0
    sample_rate_slide_shape 1
    sample_rate_slide_curve 0
    bits 8
    bits_slide 0
    bits_slide_shape 1
    bits_slide_curve 0
    cutoff 0
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0]
   [sample_rate   (varlag sample_rate sample_rate_slide sample_rate_slide_curve sample_rate_slide_shape)
    bits          (varlag bits bits_slide bits_slide_curve bits_slide_shape)
    cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff-freq   (midicps cutoff)
    use-filter    (> cutoff 0)
    [new-l new-r] (decimator [dry-l dry-r] sample_rate bits)
    wet-l         (select use-filter [new-l (lpf new-l cutoff-freq)])
    wet-r         (select use-filter [new-r (lpf new-r cutoff-freq)]) ])



 (core/def-fx sonic-pi-fx_pitch_shift
   [pitch 0
    pitch_slide 0
    pitch_slide_shape 1
    pitch_slide_curve 0

    window_size 0.2
    window_size_slide 0
    window_size_slide_shape 1
    window_size_slide_curve 0
    pitch_dis 0.0
    pitch_dis_slide 0
    pitch_dis_slide_shape 1
    pitch_dis_slide_curve 0
    time_dis 0.0
    time_dis_slide 0
    time_dis_slide_shape 1
    time_dis_slide_curve 0
    ]
   [pitch         (varlag pitch pitch_slide pitch_slide_curve pitch_slide_shape)
    window_size   (varlag window_size window_size_slide window_size_slide_curve window_size_slide_shape)
    pitch_dis     (varlag pitch_dis pitch_dis_slide pitch_dis_slide_curve pitch_dis_slide_shape)
    time_dis      (varlag time_dis time_dis_slide time_dis_slide_curve time_dis_slide_shape)
    pitch_ratio   (midiratio pitch)

    [wet-l wet-r] (pitch-shift [dry-l dry-r] window_size pitch_ratio pitch_dis time_dis)])


 (core/def-fx sonic-pi-fx_gverb
   [
    spread 0.5
    spread_slide 0
    spread_slide_shape 1
    spread_slide_curve 0
    damp 0.5
    damp_slide 0
    damp_slide_shape 1
    damp_slide_curve 0
    pre_damp 0.5
    pre_damp_slide 0
    pre_damp_slide_shape 1
    pre_damp_slide_curve 0
    dry 1
    dry_slide 0
    dry_slide_shape 1
    dry_slide_curve 0

    room [10 :ir]
    max_room [-1 :ir]
    release [3 :ir]
    ref_level [0.7 :ir]
    tail_level [0.5 :ir]

    ]
   [spread      (varlag spread spread_slide spread_slide_curve spread_slide_shape)
    dry         (varlag dry dry_slide dry_slide_curve dry_slide_shape)
    damp        (varlag damp damp_slide damp_slide_curve damp_slide_shape)
    pre_damp    (varlag pre_damp pre_damp_slide pre_damp_slide_curve pre_damp_slide_shape)
    max_room    (select:kr (= -1 max_room) [max_room (+ room 1)])
    [l-l l-r]   (* amp (g-verb dry-l room release damp pre_damp 0 dry ref_level  tail_level max_room))
    [r-l r-r]   (* amp (g-verb dry-r room release damp pre_damp 0 dry ref_level tail_level max_room))
    spr-lin     (lin-lin spread 0 1 -1 0)
    wet-l       (x-fade2 l-l r-l spr-lin)
    wet-r       (x-fade2 r-r l-r spr-lin)]
   )


 (core/def-fx sonic-pi-fx_reverb

   [mix  0.4 ;; override default mix of 1
    room 0.6
    room_slide 0
    room_slide_shape 1
    room_slide_curve 0
    damp 0.5
    damp_slide 0
    damp_slide_shape 1
    damp_slide_curve 0]

   [room          (varlag room room_slide room_slide_curve room_slide_shape)
    damp          (varlag damp damp_slide damp_slide_curve damp_slide_shape)
    rev-mix       (lin-lin fx-arg-mix -1 1 0 1)
    [wet-l wet-r] (free-verb2 dry-l dry-r rev-mix room damp)])




 (core/def-fx sonic-pi-fx_level
   []
   [wet-l dry-l
    wet-r dry-r])


 (core/def-fx sonic-pi-fx_echo
   [phase 0.25
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    decay 2
    decay_slide 0
    decay_slide_shape 1
    decay_slide_curve 0
    max_phase 2

    ]
   [phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    decay         (varlag decay decay_slide decay_slide_curve decay_slide_shape)
    [wet-l wet-r] (+ [dry-l dry-r] (comb-n [dry-l dry-r] max_phase phase decay))])




 (core/def-fx sonic-pi-fx_slicer
   [phase 0.25
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    amp_min 0
    amp_min_slide 0
    amp_min_slide_shape 1
    amp_min_slide_curve 0
    amp_max 1
    amp_max_slide 0
    amp_max_slide_shape 1
    amp_max_slide_curve 0
    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 1
    pulse_width_slide_curve 0
    smooth 0
    smooth_slide 0
    smooth_slide_shape 1
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 1
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 1
    smooth_down_slide_curve 0
    probability 0
    probability_slide 0
    probability_slide_shape 1
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 1
    prob_pos_slide_curve 0
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    seed 0
    rand_buf 0]
   [phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    amp_min             (varlag amp_min amp_min_slide amp_min_slide_curve amp_min_slide_shape)
    amp_max             (varlag amp_max amp_max_slide amp_max_slide_curve amp_max_slide_shape)
    smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
    smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
    smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
    rate                (/ 1 phase)

    prob_pos            (clip prob_pos 0 1)
    probability         (clip probability 0 1)
    prob_pos            (lin-lin prob_pos 0 1 -1 1)
    probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
    prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
    use-prob            (> probability 0)

    pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
    double_phase_offset (* 2 phase_offset)



    ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                         (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                         (lf-tri:kr rate (+ double_phase_offset 1))
                                         (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

    ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                 (impulse:kr rate))



    ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
    ctl-wave            (* -1 ctl-wave-mul ctl-wave)
    ctl-wave            (select:kr use-prob [ctl-wave
                                             (select:kr ctl-wave-prob [prob_pos ctl-wave])])
    slice-amp           (lin-lin ctl-wave -1 1 amp_min amp_max)

    slice-amp           (lag-ud slice-amp smooth_up smooth_down)
    slice-amp           (lag slice-amp smooth)
    [wet-l wet-r]       (* slice-amp [dry-l dry-r]) ])


 (core/def-fx sonic-pi-fx_wobble
   [phase 0.5
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    cutoff_min 60
    cutoff_min_slide 0
    cutoff_min_slide_shape 1
    cutoff_min_slide_curve 0
    cutoff_max 120
    cutoff_max_slide 0
    cutoff_max_slide_shape 1
    cutoff_max_slide_curve 0
    res 0.8
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    pulse_width 0.5                     ; only for pulse wave
    pulse_width_slide 0                 ; only for pulse wave
    pulse_width_slide_shape 1           ; only for pulse wa_shape 5
    pulse_width_slide_curve 0           ; only for pulse wa_curve 0
    filter 0                            ;0=rlpf, 1=rhpf
    smooth 0
    smooth_slide 0
    smooth_slide_shape 1
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 1
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 1
    smooth_down_slide_curve 0
    phase_offset 0
    wave 0                              ;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0                       ;0=normal wave, 1=inverted wave
    probability 0
    probability_slide 0
    probability_slide_shape 1
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 1
    prob_pos_slide_curve 0
    seed 0
    rand_buf 0]
   [phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    cutoff_min          (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
    cutoff_max          (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
    smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
    smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
    smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
    rate                (/ 1 phase)

    prob_pos            (clip prob_pos 0 1)
    probability         (clip probability 0 1)
    prob_pos            (lin-lin prob_pos 0 1 -1 1)
    probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
    prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
    use-prob            (> probability 0)

    pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
    double_phase_offset (* 2 phase_offset)
    res                 (lin-lin res 1 0 0 1)
    res                 (varlag res res_slide res_slide_curve res_slide_shape)


    cutoff_min          (midicps cutoff_min)
    cutoff_max          (midicps cutoff_max)



    ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                         (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                         (lf-tri:kr rate (+ double_phase_offset 1))
                                         (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

    ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                 (impulse:kr rate))

    ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
    ctl-wave            (* -1 ctl-wave-mul ctl-wave)
    ctl-wave            (select:kr use-prob [ctl-wave
                                             (select:kr ctl-wave-prob [prob_pos ctl-wave])])

    cutoff-freq         (lin-exp:kr ctl-wave -1 1 cutoff_min cutoff_max)

    cutoff-freq         (lag-ud cutoff-freq smooth_up smooth_down)
    cutoff-freq         (lag cutoff-freq smooth)

    wet-l               (select:ar filter [(rlpf dry-l cutoff-freq res)
                                           (rhpf dry-l cutoff-freq res)])
    wet-r               (select:ar filter [(rlpf dry-r cutoff-freq res)
                                           (rhpf dry-r cutoff-freq res)])])

 (core/def-fx sonic-pi-fx_panslicer
   [phase 0.25
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    pan_min -1
    pan_min_slide 0
    pan_min_slide_shape 1
    pan_min_slide_curve 0
    pan_max 1
    pan_max_slide 0
    pan_max_slide_shape 1
    pan_max_slide_curve 0
    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 1
    pulse_width_slide_curve 0
    smooth 0
    smooth_slide 0
    smooth_slide_shape 1
    smooth_slide_curve 0
    smooth_up 0
    smooth_up_slide 0
    smooth_up_slide_shape 1
    smooth_up_slide_curve 0
    smooth_down 0
    smooth_down_slide 0
    smooth_down_slide_shape 1
    smooth_down_slide_curve 0
    probability 0
    probability_slide 0
    probability_slide_shape 1
    probability_slide_curve 0
    prob_pos 0
    prob_pos_slide 0
    prob_pos_slide_shape 1
    prob_pos_slide_curve 0
    phase_offset 0
    wave 1         ;;0=saw, 1=pulse, 2=tri, 3=sine
    invert_wave 0  ;;0=normal wave, 1=inverted wave
    seed 0
    rand_buf 0]
   [phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    pan_min             (varlag pan_min pan_min_slide pan_min_slide_curve pan_min_slide_shape)
    pan_max             (varlag pan_max pan_max_slide pan_max_slide_curve pan_max_slide_shape)
    smooth              (varlag smooth smooth_slide smooth_slide_curve smooth_slide_shape)
    smooth_up           (varlag smooth_up smooth_up_slide smooth_up_slide_curve smooth_up_slide_shape)
    smooth_down         (varlag smooth_down smooth_down_slide smooth_down_slide_curve smooth_down_slide_shape)
    rate                (/ 1 phase)
    pan_min             (clip pan_min -1 1)
    pan_max             (clip pan_max -1 1)
    prob_pos            (clip prob_pos 0 1)
    probability         (clip probability 0 1)
    prob_pos            (lin-lin prob_pos 0 1 -1 1)
    probability         (varlag probability probability_slide probability_slide_curve probability_slide_shape)
    prob_pos            (varlag prob_pos prob_pos_slide prob_pos_slide_curve prob_pos_slide_shape)
    use-prob            (> probability 0)

    pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
    double_phase_offset (* 2 phase_offset)

    ctl-wave            (select:kr wave [(* -1 (lf-saw:kr rate (+ double_phase_offset 1)))
                                         (- (* 2 (lf-pulse:kr rate phase_offset pulse_width)) 1)
                                         (lf-tri:kr rate (+ double_phase_offset 1))
                                         (sin-osc:kr rate (* (+ phase_offset 0.25) (* Math/PI 2)))])

    ctl-wave-prob       (core/buffered-coin-gate rand_buf seed probability
                                                 (impulse:kr rate))



    ctl-wave-mul        (- (* 2 (> invert_wave 0)) 1)
    ctl-wave            (* -1 ctl-wave-mul ctl-wave)
    ctl-wave            (select:kr use-prob [ctl-wave
                                             (select:kr ctl-wave-prob [prob_pos ctl-wave])])

    pan-val             (lin-lin:kr ctl-wave -1 1 pan_min pan_max)
    pan-val             (lag-ud pan-val smooth_up smooth_down)
    pan-val             (lag pan-val smooth)
    [wet-l wet-r]       (balance2 dry-l dry-r pan-val amp)])


 (core/def-fx sonic-pi-fx_ixi_techno
   [phase 4
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    phase_offset 0
    cutoff_min 60
    cutoff_min_slide 0
    cutoff_min_slide_shape 1
    cutoff_min_slide_curve 0
    cutoff_max 120
    cutoff_max_slide 0
    cutoff_max_slide_shape 1
    cutoff_max_slide_curve 0
    res 0.8
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [phase         (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    rate          (/ 1 phase)
    cutoff_min    (varlag cutoff_min cutoff_min_slide cutoff_min_slide_curve cutoff_min_slide_shape)
    cutoff_max    (varlag cutoff_max cutoff_max_slide cutoff_max_slide_curve cutoff_max_slide_shape)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)
    cutoff_min    (midicps cutoff_min)
    cutoff_max    (midicps cutoff_max)
    freq          (lin-exp (sin-osc:kr rate (* (- phase_offset 0.25) (* Math/PI 2))) -1 1 cutoff_min cutoff_max)

    [wet-l wet-r] (rlpf [dry-l dry-r] freq res)])


 (core/def-fx sonic-pi-fx_compressor
   [threshold 0.2
    threshold_slide 0
    threshold_slide_shape 1
    threshold_slide_curve 0
    clamp_time 0.01
    clamp_time_slide 0
    clamp_time_slide_shape 1
    clamp_time_slide_curve 0
    slope_above 0.5
    slope_above_slide 0
    slope_above_slide_shape 1
    slope_above_slide_curve 0
    slope_below 1
    slope_below_slide 0
    slope_below_slide_shape 1
    slope_below_slide_curve 0
    relax_time 0.01
    relax_time_slide 0
    relax_time_slide_shape 1
    relax_time_slide_curve 0]

   [threshold                 (varlag threshold threshold_slide threshold_slide_curve threshold_slide_shape)
    clamp_time                (varlag clamp_time clamp_time_slide clamp_time_slide_curve clamp_time_slide_shape)
    slope_above               (varlag slope_above slope_above_slide slope_above_slide_curve slope_above_slide_shape)
    slope_below               (varlag slope_below slope_below_slide slope_below_slide_curve slope_below_slide_shape)
    relax_time                (varlag relax_time relax_time_slide relax_time_slide_curve relax_time_slide_shape)

    control-sig               (/ (+ dry-l dry-r) 2)

    [wet-l wet-r]             (compander [dry-l dry-r] control-sig threshold
                                         slope_below slope_above
                                         clamp_time relax_time)
    ])

 (core/def-fx sonic-pi-fx_band_eq
   [freq 100
    freq_slide 0
    freq_slide_shape 1
    freq_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0
    db 0.6
    db_slide 0
    db_slide_shape 1
    db_slide_curve 0]
   [res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)
    db            (varlag db db_slide db_slide_curve db_slide_shape)
    freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
    freq          (midicps freq)

    [wet-l wet-r] (mid-eq [dry-l dry-r] freq res db)
    ])


 (core/def-fx sonic-pi-fx_rlpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0.5
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)

    [wet-l wet-r] (rlpf [dry-l dry-r] cutoff res)])


 )

;; need to break these up due to Clojure's method size limitation
(without-namespace-in-synthdef
  (core/def-fx sonic-pi-fx_nrlpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0.5
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)

    [wet-l wet-r] (normalizer (rlpf [dry-l dry-r] cutoff res))
    ])


 (core/def-fx sonic-pi-fx_rhpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0.5
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)
    [wet-l wet-r] (rhpf [dry-l dry-r] cutoff res)])



 (core/def-fx sonic-pi-fx_nrhpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0
    res 0.5
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)
    res           (varlag res res_slide res_slide_curve res_slide_shape)
    [wet-l wet-r] (normalizer (rhpf [dry-l dry-r] cutoff res))])


 (core/def-fx sonic-pi-fx_hpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)

    [wet-l wet-r] (hpf [dry-l dry-r] cutoff)])


 (core/def-fx sonic-pi-fx_nhpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)

    [wet-l wet-r] (normalizer (hpf [dry-l dry-r] cutoff))])


 (core/def-fx sonic-pi-fx_lpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)

    [wet-l wet-r] (lpf [dry-l dry-r] cutoff)])


 (core/def-fx sonic-pi-fx_nlpf
   [cutoff 100
    cutoff_slide 0
    cutoff_slide_shape 1
    cutoff_slide_curve 0]
   [cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
    cutoff        (midicps cutoff)

    [wet-l wet-r] (normalizer (lpf [dry-l dry-r] cutoff))])


 (core/def-fx sonic-pi-fx_normaliser
   [level 1
    level_slide 0
    level_slide_shape 1
    level_slide_curve 0]
   [level         (varlag level level_slide level_slide_curve level_slide_shape)

    [wet-l wet-r] (normalizer [dry-l dry-r] level)])


 (core/def-fx sonic-pi-fx_distortion
   [distort 0.5
    distort_slide 0
    distort_slide_shape 1
    distort_slide_curve 0]
   [distort       (varlag distort distort_slide distort_slide_curve distort_slide_shape)
    k             (/ (* 2 distort) (- 1 distort))

    [wet-l wet-r] (/ (* [dry-l dry-r] (+ 1 k)) (+ 1 (* k (abs [dry-l dry-r]))))])


 (core/def-fx sonic-pi-fx_pan
   [pan 0
    pan_slide 0
    pan_slide_shape 1
    pan_slide_curve 0]
   [pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
    [wet-l wet-r] (balance2 dry-l dry-r pan amp)])


 (core/def-fx sonic-pi-fx_bpf
   [centre 100
    centre_slide 0
    centre_slide_shape 1
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
    freq          (midicps centre)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)

    [wet-l wet-r] (bpf [dry-l dry-r] freq res)])



 (core/def-fx sonic-pi-fx_rbpf
   [centre 100
    centre_slide 0
    centre_slide_shape 1
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
    freq          (midicps centre)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)

    [wet-l wet-r] (resonz [dry-l dry-r] freq res)])

 (core/def-fx sonic-pi-fx_nrbpf
   [centre 100
    centre_slide 0
    centre_slide_shape 1
    centre_slide_curve 0
    res 0.6
    res_slide 0
    res_slide_shape 1
    res_slide_curve 0]
   [centre        (varlag centre centre_slide centre_slide_curve centre_slide_shape)
    freq          (midicps centre)
    res           (lin-lin res 1 0 0 1)
    res           (varlag res res_slide res_slide_curve res_slide_shape)

    [wet-l wet-r] (normalizer (resonz [dry-l dry-r] freq res))])

 (core/def-fx sonic-pi-fx_octaver
   [super_amp 1
    super_amp_slide 0
    super_amp_slide_shape 1
    super_amp_slide_curve 0
    sub_amp 1
    sub_amp_slide 0
    sub_amp_slide_shape 1
    sub_amp_slide_curve 0
    subsub_amp 1
    subsub_amp_slide 0
    subsub_amp_slide_shape 1
    subsub_amp_slide_curve 0]
   [super_amp     (varlag super_amp super_amp_slide super_amp_slide_curve super_amp_slide_shape)
    sub_amp       (varlag sub_amp sub_amp_slide sub_amp_slide_curve sub_amp_slide_shape)
    subsub_amp    (varlag subsub_amp subsub_amp_slide subsub_amp_slide_curve subsub_amp_slide_shape)
    direct-lpf    (lpf [dry-l dry-r] 440)
    super-oct     (* 2 (leak-dc (abs direct-lpf))) ;; Compensate for resulting wave being half amplitude
    sub-oct       (toggle-ff:ar direct-lpf)
    sub-sub-oct   (toggle-ff:ar sub-oct)

    [wet-l wet-r] (+ (* super-oct super_amp) (* direct-lpf sub-oct sub_amp) (* direct-lpf sub-sub-oct subsub_amp))])

 (core/def-fx sonic-pi-fx_ring_mod
   [freq 30
    freq_slide 0
    freq_slide_shape 1
    freq_slide_curve 0
    mod_amp 1
    mod_amp_slide 0
    mod_amp_slide_shape 1
    mod_amp_slide_curve 0]
   [mod_amp       (varlag mod_amp mod_amp_slide mod_amp_slide_curve mod_amp_slide_shape)
    freq          (varlag freq freq_slide freq_slide_curve freq_slide_shape)
    freq          (midicps freq)

    [wet-l wet-r] (limiter (ring1 [dry-l dry-r] (* mod_amp (sin-osc freq))))])

 (core/def-fx sonic-pi-fx_flanger
   [phase 4
    phase_slide 0
    phase_slide_shape 1
    phase_slide_curve 0
    phase_offset 0
    wave 4
    invert_wave 0
    stereo_invert_wave 0

    pulse_width 0.5
    pulse_width_slide 0
    pulse_width_slide_shape 1
    pulse_width_slide_curve 0

    delay 5
    delay_slide 0
    delay_slide_shape 1
    delay_slide_curve 0

    max_delay 20

    depth 5
    depth_slide 0
    depth_slide_shape 1
    depth_slide_curve 0

    feedback 0
    feedback_slide 0
    feedback_slide_shape 1
    feedback_slide_curve 0

    decay 2
    decay_slide 0
    decay_slide_shape 1
    decay_slide_curve 0

    invert_flange 0]
   [phase               (varlag phase phase_slide phase_slide_curve phase_slide_shape)
    pulse_width         (varlag pulse_width pulse_width_slide pulse_width_slide_curve pulse_width_slide_shape)
    delay               (varlag delay delay_slide delay_slide_curve delay_slide_shape)
    depth               (varlag depth depth_slide depth_slide_curve depth_slide_shape)
    feedback            (varlag feedback feedback_slide feedback_slide_curve feedback_slide_shape)
    decay               (varlag decay decay_slide decay_slide_curve decay_slide_shape)
    rate                (/ 1 phase)

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

    delay-l             (allpass-c  (+ (limiter (* local-l feedback)) (* (- 1 feedback) dry-l))
                                    (/ max_delay 1000)
                                    (max
                                     (mul-add ctl-wave-l (/ depth 1000) (/ delay 1000))
                                     0)
                                    (/ decay 1000))

    delay-r             (allpass-c  (+ (limiter (* local-r feedback)) (* (- 1 feedback) dry-r))
                                    (/ max_delay 1000)
                                    (max
                                     (mul-add ctl-wave-r (/ depth 1000) (/ delay 1000))
                                     0)
                                    (/ decay 1000))

    flange-wave-mul     (+ (* -2 (> invert_flange 0)) 1)
    wet-l               (+ dry-l (* flange-wave-mul delay-l))
    wet-r               (+ dry-r (* flange-wave-mul delay-r))
    lout                (local-out:ar [wet-l wet-r])])




)

 (comment
   (core/save-synthdef sonic-pi-fx_echo)
   (core/save-synthdef sonic-pi-fx_reverb)
   (core/save-synthdef sonic-pi-fx_mono)
   (core/save-synthdef sonic-pi-fx_vowel)

   (core/save-synthdef sonic-pi-fx_krush)
   (core/save-synthdef sonic-pi-fx_bitcrusher)

   (core/save-synthdef sonic-pi-fx_gverb)
   (core/save-synthdef sonic-pi-fx_level)

   (core/save-synthdef sonic-pi-fx_slicer)
   (core/save-synthdef sonic-pi-fx_panslicer)
   (core/save-synthdef sonic-pi-fx_wobble)
   (core/save-synthdef sonic-pi-fx_ixi_techno)
   (core/save-synthdef sonic-pi-fx_compressor)
   (core/save-synthdef sonic-pi-fx_band_eq)
   (core/save-synthdef sonic-pi-fx_rlpf)
   (core/save-synthdef sonic-pi-fx_nrlpf)
   (core/save-synthdef sonic-pi-fx_rhpf)
   (core/save-synthdef sonic-pi-fx_nrhpf)
   (core/save-synthdef sonic-pi-fx_hpf)
   (core/save-synthdef sonic-pi-fx_nhpf)
   (core/save-synthdef sonic-pi-fx_lpf)
   (core/save-synthdef sonic-pi-fx_nlpf)
   (core/save-synthdef sonic-pi-fx_normaliser)
   (core/save-synthdef sonic-pi-fx_distortion)
   (core/save-synthdef sonic-pi-fx_pan)
   (core/save-synthdef sonic-pi-fx_bpf)
   (core/save-synthdef sonic-pi-fx_rbpf)
   (core/save-synthdef sonic-pi-fx_nrbpf)
   (core/save-synthdef sonic-pi-fx_tanh)
   (core/save-synthdef sonic-pi-fx_whammy)
   (core/save-synthdef sonic-pi-fx_pitch_shift)
   (core/save-synthdef sonic-pi-fx_ring_mod)
   (core/save-synthdef sonic-pi-fx_octaver)
   (core/save-synthdef sonic-pi-fx_flanger))
