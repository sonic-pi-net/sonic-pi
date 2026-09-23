# amp and pan on the basic player
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/amp_and_pan.rb
load_sample :loop_amen
sample :loop_amen, amp: 0.5
sample :loop_amen, amp: 0
sample :loop_amen, pan: -0.5
