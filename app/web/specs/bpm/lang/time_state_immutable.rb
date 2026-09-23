# what Time State keeps, nothing can change: a list comes back frozen, a hash as a map, a string frozen, a ring a ring
use_bpm 120   # at twice the tempo: specs/lang/time_state_immutable.rb
set :l, [1, 2]
puts get(:l)
puts get(:l).frozen?
set :h, {a: 1, b: [2, 3], c: "café"}
puts get(:h)
set :s, "hi"
puts get(:s).frozen?
set :r, (ring 1, 2.5, :e4)
puts get(:r)
set :n, [nil, true, false, 3.25, 2**70]
puts get(:n)
begin
  get(:l) << 3
rescue => e
  puts e.class
end
cue :go, 1, [2, 3], "x"
