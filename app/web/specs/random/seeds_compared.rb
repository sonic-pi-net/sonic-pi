# a few seeds, each first three draws
[0, 1, 2, 3, 100, 12345].each do |s|
  use_random_seed s
  puts [rand, rand_i(100), rrand_i(1, 6)]
end
