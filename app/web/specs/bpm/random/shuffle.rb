# shuffle reorders using the stream
use_bpm 120   # at twice the tempo: specs/random/shuffle.rb
puts shuffle([1, 2, 3, 4, 5])
puts [1, 2, 3, 4, 5].shuffle
puts (ring 1, 2, 3, 4).shuffle
