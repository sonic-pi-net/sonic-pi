# spawning a thread does not advance the parent's time
use_bpm 120   # at twice the tempo: specs/in_thread/parent_time_unaffected.rb
in_thread do
  sleep 1
end
play 60
