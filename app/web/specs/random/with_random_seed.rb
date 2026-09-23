# with_random_seed scopes the seed and restores the stream after the block
use_random_seed 7
puts rand
with_random_seed 99 do
  puts rand
  puts rand
end
puts rand
