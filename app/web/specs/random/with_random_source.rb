# with_random_source scopes the source
use_random_seed 0
puts rand
with_random_source :perlin do
  puts rand
end
puts rand
