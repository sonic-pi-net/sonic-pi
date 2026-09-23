# use_random_seed restarts the stream; the same seed gives the same values
use_bpm 120   # at twice the tempo: specs/random/use_random_seed.rb
use_random_seed 42
a = [rand, rand, rand]
use_random_seed 42
b = [rand, rand, rand]
puts a
puts a == b
use_random_seed 1000
puts rand
