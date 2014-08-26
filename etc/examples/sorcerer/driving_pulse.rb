load_sample :drum_heavy_kick

define :drums do
  sample :drum_heavy_kick, rate: 0.75
  sleep 0.5
  sample :drum_heavy_kick
  sleep 0.5
end

define :synths do
  use_synth :mod_pulse
  use_synth_defaults amp: 1, mod_range: 15, cutoff: 80, pulse_width: 0.2, attack: 0.03, release: 0.6,  mod_phase: 0.25
  play 30
  sleep 0.25
  play 38
  sleep 0.25
end

in_thread(name: :drums){loop{drums}}
in_thread(name: :synths){loop{synths}}
