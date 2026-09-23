# set and get: a value set at a time is seen from that time on, in any thread; sync wakes on set
use_bpm 120   # at twice the tempo: specs/lang/time_state.rb
set :foo, 1
puts get(:foo)
puts get(:bar)
puts get(:bar, 7)
puts get[:foo]
in_thread do
  sleep 0.5
  set :foo, 2
  sleep 0.5
  set :baz, [1, 2]
end
sleep 0.25
puts get(:foo)
sleep 0.5
puts get(:foo)
v = sync :baz
puts v
puts get(:baz)
