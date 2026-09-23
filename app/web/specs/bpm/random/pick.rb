# pick draws several, with replacement
use_bpm 120   # at twice the tempo: specs/random/pick.rb
puts pick([1, 2, 3, 4, 5], 3)
puts [1, 2, 3, 4, 5].pick(2)
puts (ring 1, 2, 3).pick
