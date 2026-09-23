# use_sample_bpm sets the tempo so one beat is the sample's length
use_bpm 120   # at twice the tempo: specs/sample/use_sample_bpm.rb
use_sample_bpm :loop_amen
sample :loop_amen
sleep 1
sample :loop_amen
puts current_bpm
