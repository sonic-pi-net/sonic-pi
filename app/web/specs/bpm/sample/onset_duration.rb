# sample_duration with onset:
use_bpm 120   # at twice the tempo: specs/sample/onset_duration.rb
puts sample_duration(:loop_amen, onset: 0)
puts sample_duration(:loop_amen, onset: 1)
puts sample_duration(:loop_amen, onset: 1, rate: 2)
