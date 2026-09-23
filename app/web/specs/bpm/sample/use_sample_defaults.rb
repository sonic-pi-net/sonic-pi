# use_sample_defaults applies to every later sample; explicit opts win
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/use_sample_defaults.rb
load_sample :loop_amen
use_sample_defaults amp: 0.5, rate: 0.5
sample :loop_amen
sample :loop_amen, rate: 1
with_sample_defaults amp: 0.1 do
  sample :loop_amen
end
use_merged_sample_defaults cutoff: 70
sample :loop_amen
