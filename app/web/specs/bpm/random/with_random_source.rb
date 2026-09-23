# with_random_source scopes the source
use_bpm 120   # at twice the tempo: specs/random/with_random_source.rb
use_random_seed 0
puts rand
with_random_source :perlin do
  puts rand
end
puts rand
