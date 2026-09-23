# a string is a ring of its characters, as Sonic Pi's patterns use them; it shuffles too
puts "x--x".ring
puts "x--x".ring.tick == "x"
puts "x--x".ring.tick
puts "abc".ring.reverse
use_random_seed 3
puts "abcdef".shuffle
