# sample_duration reports the length in beats, respecting rate and tempo
puts sample_duration(:loop_amen)
puts sample_duration(:loop_amen, rate: 2)
puts sample_duration(:loop_amen, rate: -0.5)
puts sample_duration(:loop_amen, start: 0.25, finish: 0.75)
puts sample_duration(:loop_amen, beat_stretch: 3)
puts sample_duration(:loop_amen, attack: 0.1, release: 0.5, sustain: 0)
use_bpm 120
puts sample_duration(:loop_amen)
puts sample_duration(:drum_heavy_kick)
