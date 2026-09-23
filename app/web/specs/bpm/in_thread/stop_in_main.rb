# stop in the main thread ends it; spawned threads finish on their own
use_bpm 120   # at twice the tempo: specs/in_thread/stop_in_main.rb
in_thread do
  sleep 0.5
  play 60
end
stop
play 99
