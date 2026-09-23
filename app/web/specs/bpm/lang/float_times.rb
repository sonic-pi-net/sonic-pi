# a Float counts: density below one, a Float's times, and a Float reps: on with_fx
use_bpm 120   # at twice the tempo: specs/lang/float_times.rb
density 0.5 do
  play 60
  sleep 1
end
play 62
2.5.times do |i|
  play 70 + i
  sleep 0.25
end
with_fx :echo, reps: 2.0 do
  play 72
  sleep 0.5
end
