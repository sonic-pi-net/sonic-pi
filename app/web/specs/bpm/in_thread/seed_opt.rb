# seed: sets the thread's random stream
use_bpm 120   # at twice the tempo: specs/in_thread/seed_opt.rb
in_thread(seed: 1) do
  play rrand_i(60, 80)
  play rrand_i(60, 80)
end
in_thread(seed: 1) do
  play rrand_i(60, 80)
end
