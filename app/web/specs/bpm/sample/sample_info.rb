# sample_info reports frames, channels and rate
use_bpm 120   # at twice the tempo: specs/sample/sample_info.rb
i = sample_info(:loop_amen)
puts i.num_frames
puts i.num_chans
puts i.sample_rate
puts i.duration
