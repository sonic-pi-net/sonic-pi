# pitch shifts via the full player; rpitch changes the rate instead
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/pitch.rb
load_sample :loop_amen
sample :loop_amen, pitch: 3
sample :loop_amen, pitch: -12
sample :loop_amen, rpitch: 12
sample :loop_amen, rpitch: -7
