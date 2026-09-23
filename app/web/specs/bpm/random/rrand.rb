# rrand and rrand_i draw within a range, inclusive
use_bpm 120   # at twice the tempo: specs/random/rrand.rb
puts rrand(0, 10)
puts rrand(-1, 1)
puts rrand(5, 5)
puts rrand_i(1, 6)
puts rrand_i(60, 72)
puts rrand_i(3, 3)
