# with_random_seed scopes the seed and restores the stream after the block
use_bpm 120   # at twice the tempo: specs/random/with_random_seed.rb
use_random_seed 7
puts rand
with_random_seed 99 do
  puts rand
  puts rand
end
puts rand
