# beat_stretch combined with an explicit rate
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/beat_stretch_with_rate.rb
load_sample :loop_amen
sample :loop_amen, beat_stretch: 2, rate: -1
sample :loop_amen, beat_stretch: 2, rate: 0.5
