# tick counters are per thread
use_bpm 120   # at twice the tempo: specs/in_thread/tick_is_per_thread.rb
notes = (ring 60, 62, 64, 66)
in_thread do
  3.times do
    play notes.tick
    sleep 0.1
  end
end
2.times do
  play notes.tick + 12
  sleep 0.1
end
