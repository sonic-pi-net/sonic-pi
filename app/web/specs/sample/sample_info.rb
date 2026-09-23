# sample_info reports frames, channels and rate
i = sample_info(:loop_amen)
puts i.num_frames
puts i.num_chans
puts i.sample_rate
puts i.duration
