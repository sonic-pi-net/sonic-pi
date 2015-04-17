# Coded by Sam Aaron

load_samples(sample_names :ambi)
sleep 2

with_fx :reverb, mix: 0.8 do
  live_loop :foo do
    # try changing the sp_ vars..
    sp_name = choose sample_names :ambi
    # sp_name = choose sample_names :drum
    sp_time = [1, 2].choose
    #sp_time = 0.5
    sp_rate = 1
    #sp_rate = 4

    s = sample sp_name, cutoff: rrand(70, 130), rate: sp_rate * choose([0.5, 1]), pan: rrand(-1, 1), pan_slide: sp_time
    control s, pan: rrand(-1, 1)
    sleep sp_time
  end
end
