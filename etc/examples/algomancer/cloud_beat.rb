# Coded by SonicPit
#
# Taken from "Beats basteln wie die Großen"
# c't 13/2017

# Note: requires a powerful machine to run smoothly.

use_bpm 100

# HISS
live_loop :hiss_loop do
  sample :vinyl_hiss, amp: 2
  sleep sample_duration :vinyl_hiss
end

# HIHAT
define :hihat do
  use_synth :pnoise
  with_fx :hpf, cutoff: 120 do
    play release: 0.01, amp: 13
  end
end

live_loop :hihat_loop do
  divisors = ring 2, 4, 2, 2, 2, 2, 2, 6
  divisors.tick.times do
    hihat
    sleep 1.0 / divisors.look
  end
end

# SNARE
live_loop :snare_loop do
  sleep ring(2.5, 3)[tick]
  with_fx :lpf, cutoff: 100 do
    sample :sn_dub, sustain: 0, release: 0.05, amp: 3
  end
  sleep ring(1.5, 1)[look]
end

# BASSDRUM
define :bassdrum do |note1, duration, note2 = note1|
  use_synth :sine
  with_fx :hpf, cutoff: 100 do
    play note1 + 24, amp: 40, release: 0.01
  end
  with_fx :distortion, distort: 0.1, mix: 0.3 do
    with_fx :lpf, cutoff: 26 do
      with_fx :hpf, cutoff: 55 do
        bass = play note1, amp: 85, release: duration, note_slide: duration
        control bass, note: note2
      end
    end
  end
  sleep duration
end

live_loop :bassdrum_schleife do
  bassdrum 36, 1.5
  if bools(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)[tick]
    bassdrum 36, 0.5, 40
    bassdrum 38, 1, 10
  else
    bassdrum 36, 1.5
  end
  bassdrum 36, 1.0, ring(10, 10, 10, 40)[look]
end

# CHORD CONTROL
# This part provides two rings called "chord_high" and "chord_low".
# They always contain the "permitted" notes in order that everything will be in tune.
# You can use them in other live loops to select notes.

chord_1 = chord :c4, :maj9, num_octaves: 2
chord_2 = chord :es4, :maj9, num_octaves: 2
chord_3 = chord :b3, :maj9, num_octaves: 2
chord_4 = chord :d4, :maj9, num_octaves: 2

chord_low_1 = chord :c2, :maj9
chord_low_2 = chord :es2, :maj9
chord_low_3 = chord :b1, :maj9
chord_low_4 = chord :d2, :maj9

chord_high = chord_1
chord_low = chord_low_1

live_loop :chord_selector, delay: -0.5 do
  chord_high = (knit(chord_1, 2, chord_2, 2, chord_3, 4,chord_4, 4)).tick
  chord_low = (knit(chord_low_1, 2, chord_low_2, 2, chord_low_3, 4, chord_low_4, 4)).look
  sleep 8
end

# SPHERES
define :chord_player do |the_chord|
  use_synth :blade
  the_chord.each do |note|
    play note, attack: rand(4), release: rand(6..8), cutoff: rand(50..85), vibrato_rate: rand(0.01..2), amp: 0.55
  end
end

with_fx :reverb, room: 0.99, mix: 0.7 do
  live_loop :chord_loop do
    chord_player chord_high.pick(6)
    chord_player chord_low.take(3)
    sleep 8
  end
end

# Coded by Pit Noack
# supported by
# Alexander Degraf
# Astrid Hagenguth
# Enrico Mercaldi
# http://www.maschinennah.de/
# mail@pitnoack.de
