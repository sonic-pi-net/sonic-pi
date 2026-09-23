# random values are seeded per run, so this melody is the same every time
use_bpm 120   # at twice the tempo: specs/play/rand_seeded.rb
4.times do
  play rrand_i(60, 72)
  sleep 0.1
end
use_random_seed 7
play rrand_i(60, 72)
play rand(1)
