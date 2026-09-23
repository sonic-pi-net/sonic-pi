# a few seeds, each first three draws
use_bpm 120   # at twice the tempo: specs/random/seeds_compared.rb
[0, 1, 2, 3, 100, 12345].each do |s|
  use_random_seed s
  puts [rand, rand_i(100), rrand_i(1, 6)]
end
