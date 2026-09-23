# each noise source is its own table; the seed indexes into it
use_bpm 120   # at twice the tempo: specs/random/random_sources.rb
[:white, :pink, :light_pink, :dark_pink, :perlin].each do |src|
  use_random_source src
  use_random_seed 0
  puts src, [rand, rand, rand]
end
