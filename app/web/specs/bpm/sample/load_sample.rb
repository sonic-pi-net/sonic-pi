# load_sample loads without playing; the later play reuses it
use_bpm 120   # at twice the tempo: specs/sample/load_sample.rb
load_sample :loop_amen
sleep 0.25
sample :loop_amen
