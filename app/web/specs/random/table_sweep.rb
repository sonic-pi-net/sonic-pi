# A long walk through every table, summed. 50,000 draws is more than the trimmed :white table physically holds,
# so a single index that wrapped to the wrong place would change the total. The oracle reads the whole tables and
# the browser reads :white with its repeats taken off: the sums have to be the same number.
[:white, :pink, :light_pink, :dark_pink, :perlin].each do |src|
  use_random_source src
  use_random_seed 0
  total = 0
  50_000.times { total += rand }
  puts [src, total, rand, rand_i(1_000_000)]
end

# the same walk started near the end of the table, so most of it runs past the wrap
use_random_source :white
use_random_seed 440_000
sum = 0
5_000.times { sum += rand }
puts ["wrapped", sum]

# every verb that draws, at the indices where a trimmed table could differ
[0, 44_099, 44_100, 44_101, 88_200, 220_500, 440_999, 441_000].each do |seed|
  use_random_seed seed
  puts [seed, rand, rand_i(100), rrand(0, 10), rrand_i(0, 10), dice(6), one_in(3),
        choose([1, 2, 3, 4, 5]), [1, 2, 3, 4, 5].shuffle, rand_look]
end
