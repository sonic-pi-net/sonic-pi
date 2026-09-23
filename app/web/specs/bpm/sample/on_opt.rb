# on: false silences the sample
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/on_opt.rb
load_sample :loop_amen
sample :loop_amen, on: false
sample :loop_amen, on: true
