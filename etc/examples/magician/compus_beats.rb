sd = sample_duration :loop_compus

define :loopr do
  sample :loop_compus, rate: [0.5, 1, 1, 1, 1, 2].choose if rand < 0.9
  sleep sd
end

define :bass do
  sample :bass_voxy_c, amp: rrand(0.1, 0.2), rate: [0.5, 0.5, 1, 1,2,4].choose if rand < 0.25
  use_synth :mod_pulse
  play :C1, mod_range: 12, amp: rrand(0.5, 1), mod_phase: [8, 16, 32].choose / sd, release: 1, cutoff: rrand(50, 90)
  play :C2, mod_range: [24, 36, 34].choose, amp: 0.35, mod_phase: sd/16, release: 2, cutoff: 60, pulse_width: rand
  sleep sd / 4
end

in_thread(name: :t1){loop{loopr}}
in_thread(name: :t2){loop{bass}}
