# Coded by Sam Aaron

use_debug false
use_random_seed 667
load_sample :ambi_lunar_land
sleep 1

live_loop :travelling do |idx|
  with_fx :echo, phase: 0.125, mix: 0.8  do
    use_synth :beep
    notes = scale(:e3, :minor_pentatonic, num_octaves: 1)
    4.times do
      use_random_seed 67
      16.times do |idx|
        sleep 0.25
        play notes.choose, pulse_width: 0.2, attack: 0, release: 0.1, pan: (range -1, 1, 0.125)[idx], amp: rrand(1, 2)
      end
    end
  end
end

live_loop :comet, auto_cue: false do
  if rand < 0.25
    sample :ambi_lunar_land
    puts :comet_landing
  end
  sleep 8
end

live_loop :shuff, auto_cue: false do |idx|
  with_fx :hpf, cutoff: 10 do |f|
    8.times do
      sleep 0.25
      sample :bd_tek, amp: factor?(idx, 8) ? 6 : 4
      sleep 0.25
      use_synth :tb303
      use_synth_defaults cutoff_attack: 1, cutoff_release: 0, env_curve: 2
      play (knit :e2, 24, :c2, 8)[idx], release: 1.5, cutoff: (range 70, 90, 1)[idx], depth: 10 , amp: 2 if factor?(idx, 2)
      sample :sn_dub, rate: -1, sustain: 0, release: (knit 0.05, 3, 0.5, 1)[idx]
      idx += 1
    end
  end
  idx
end
