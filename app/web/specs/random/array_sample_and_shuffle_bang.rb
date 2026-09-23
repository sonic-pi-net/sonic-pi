# an array's sample and shuffle! come from Sonic Pi's random stream: the seed repeats them
use_random_seed 5
puts [1, 2, 3, 4, 5].sample
puts [1, 2, 3, 4, 5].sample
a = [1, 2, 3, 4]
a.shuffle!
puts a
use_random_seed 5
puts [1, 2, 3, 4, 5].sample
