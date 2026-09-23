# rate changes speed and pitch; negative rate plays backwards
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/rate.rb
load_sample :loop_amen
sample :loop_amen, rate: 0.5
sample :loop_amen, rate: 2
sample :loop_amen, rate: -1
sample :loop_amen, rate: -0.25
