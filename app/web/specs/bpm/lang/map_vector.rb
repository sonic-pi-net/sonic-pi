# map and vector: Sonic Pi's immutable map and list
use_bpm 120   # at twice the tempo: specs/lang/map_vector.rb
m = map(a: 1, b: 2)
puts m
puts m[:a]
v = vector(1, 2, 3)
puts v
puts v[1]
puts v[5].inspect
puts v.ring
puts map(:x, 1, :y, 2)
