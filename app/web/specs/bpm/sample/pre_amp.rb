# pre_amp and other less common opts
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/pre_amp.rb
load_sample :loop_amen
sample :loop_amen, pre_amp: 2
sample :loop_amen, norm: 1
sample :loop_amen, compress: 1, threshold: 0.5
