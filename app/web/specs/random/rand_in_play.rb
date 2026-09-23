# random notes and opts reach the synth as drawn
use_random_seed 11
3.times do
  play rrand_i(50, 70), amp: rand, pan: rrand(-1, 1)
  sleep 0.1
end
