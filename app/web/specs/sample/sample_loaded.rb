# sample_loaded? before and after; the first trigger of an unloaded sample
# runs in a helper thread, so the second question waits for it
puts sample_loaded?(:loop_amen)
sample :loop_amen
sleep 0.1
puts sample_loaded?(:loop_amen)
