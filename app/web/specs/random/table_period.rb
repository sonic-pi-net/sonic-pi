# the numbers either side of where :white repeats. Sonic Pi's :white table is one second of values written ten
# times over, so the browser is sent one copy and the index wraps on it (scripts/lib/runtime-assets.mjs,
# Rand::Table). These are the indices where a table sent short could differ from the whole one, and the oracle
# reads the whole one: they have to agree.
use_random_seed 0
puts rand

# either side of the repeat, and of the table's own end
[44_098, 44_099, 44_100, 44_101, 88_199, 88_200, 220_499, 440_998, 440_999, 441_000, 441_001].each do |i|
  use_random_seed i
  puts [i, rand, rand_i(1000)]
end

# and walked over the boundary rather than jumped to
use_random_seed 0
rand_skip 44_097
puts [rand, rand, rand, rand, rand, rand]

# the same walk in each source: only :white repeats, so the others prove the wrap changed nothing for them
[:white, :pink, :light_pink, :dark_pink, :perlin].each do |src|
  use_random_source src
  use_random_seed 44_099
  puts [src, rand, rand, rand]
end
