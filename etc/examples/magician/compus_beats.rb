# Coded by Sam Aaron

use_sample_bpm :loop_compus, num_beats: 4

live_loop :loopr do
  sample :loop_compus, rate: [0.5, 1, 1, 1, 1, 2].choose unless one_in(10)
  sleep 4
end

live_loop :bass do
  sample :bass_voxy_c, amp: rrand(0.1, 0.2), rate: [0.5, 0.5, 1, 1,2,4].choose if one_in(4)
  use_synth :mod_pulse
  use_synth_defaults mod_invert_wave: 1
  play :C1, mod_range: 12, amp: rrand(0.5, 1), mod_phase: [0.25, 0.5, 1].choose, release: 1, cutoff: rrand(50, 90)
  play :C2, mod_range: [24, 36, 34].choose, amp: 0.35, mod_phase: 0.25, release: 2, cutoff: 60, pulse_width: rand
  sleep 1
end
