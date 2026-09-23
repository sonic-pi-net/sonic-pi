# counters are per thread and a child starts fresh
use_bpm 120   # at twice the tempo: specs/tick/per_thread.rb
tick
tick
in_thread do
  puts look
  puts tick
end
puts look
