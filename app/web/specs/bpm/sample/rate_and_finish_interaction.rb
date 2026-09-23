# finish with a negative rate plays that portion backwards
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/rate_and_finish_interaction.rb
load_sample :loop_amen
sample :loop_amen, finish: 0.5, rate: -1
sample :loop_amen, start: 0.5, rate: -2
