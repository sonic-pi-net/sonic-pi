# choose picks one element; the stream advances each time
use_bpm 120   # at twice the tempo: specs/random/choose.rb
puts choose([1, 2, 3, 4, 5])
puts choose([1, 2, 3, 4, 5])
puts choose([:a, :b, :c])
puts [1, 2, 3].choose
puts (ring 10, 20, 30).choose
