# onset: alongside start: and finish:
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/onset_with_start_finish.rb
load_sample :loop_amen
sample :loop_amen, onset: 1, start: 0.5
sample :loop_amen, onset: 1, finish: 0.1
