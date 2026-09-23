# in_thread returns to the parent without running the block first
use_bpm 120   # at twice the tempo: specs/in_thread/returns_immediately.rb
x = 1
in_thread do
  x = 2
  sleep 0.1
end
puts x
sleep 0.2
puts x
