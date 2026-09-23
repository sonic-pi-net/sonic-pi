# each noise source is its own table; the seed indexes into it
[:white, :pink, :light_pink, :dark_pink, :perlin].each do |src|
  use_random_source src
  use_random_seed 0
  puts src, [rand, rand, rand]
end
