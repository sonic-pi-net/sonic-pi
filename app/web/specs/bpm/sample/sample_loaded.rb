# sample_loaded? before and after; the first trigger of an unloaded sample
# runs in a helper thread, so the second question waits for it
use_bpm 120   # at twice the tempo: specs/sample/sample_loaded.rb
puts sample_loaded?(:loop_amen)
sample :loop_amen
sleep 0.1
puts sample_loaded?(:loop_amen)
