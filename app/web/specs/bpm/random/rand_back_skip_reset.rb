# rand_back rewinds, rand_skip jumps ahead, rand_reset returns to the seed
use_bpm 120   # at twice the tempo: specs/random/rand_back_skip_reset.rb
a = rand
b = rand
rand_back
puts rand == b
rand_back 2
puts rand == a
rand_skip
puts rand
rand_reset
puts rand == a
