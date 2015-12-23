# Coded by Sam Aaron

use_debug false

with_fx :reverb do
  live_loop :choral do
    r = (ring 0.5, 1.0/3, 3.0/5).choose
    cue :choir, rate: r
    8.times do
      sample :ambi_choir, rate: r, pan: rrand(-1, 1)
      sleep 0.5
    end
  end
end


live_loop :wub_wub do
  with_fx :wobble, phase: 2, reps: 16 do |w|
    with_fx :echo, mix: 0.6 do
      sample :drum_heavy_kick
      sample :bass_hit_c, rate: 0.8, amp: 0.4
      sleep 1
      ## try changing the wobble's phase duration:
      # control w, phase: (ring 0.5, 1, 2).choose
    end
  end
end
