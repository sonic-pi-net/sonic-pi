# start and finish beyond 0..1
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/start_finish_out_of_range.rb
load_sample :loop_amen
sample :loop_amen, start: -0.5
sample :loop_amen, finish: 1.5
