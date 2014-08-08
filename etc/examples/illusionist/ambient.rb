load_samples(sample_names :ambi)

with_fx :reverb, mix: 0.3 do
  loop do
    s_name = choose sample_names :ambi
    s_time = [1, 2].choose
    s = sample s_name, rate: choose([0.5, 1]), pan: rrand(-1, 1), pan_slide: s_time
    control s, pan: rrand(-1, 1)
    sleep s_time
  end
end
