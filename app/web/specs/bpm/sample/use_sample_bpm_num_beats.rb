# num_beats: divides the sample into beats
use_bpm 120   # at twice the tempo: specs/sample/use_sample_bpm_num_beats.rb
use_sample_bpm :loop_amen, num_beats: 4
sample :loop_amen
sleep 4
play 60
puts current_bpm
