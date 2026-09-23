# rdist draws around a centre
use_bpm 120   # at twice the tempo: specs/random/rdist.rb
puts rdist(1)
puts rdist(2, 60)
puts rdist(0.5, 0, step: 0.1)
