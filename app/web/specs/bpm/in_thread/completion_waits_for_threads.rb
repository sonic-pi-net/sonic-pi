# the run is over only when every thread has finished
use_bpm 120   # at twice the tempo: specs/in_thread/completion_waits_for_threads.rb
in_thread do
  sleep 1
  play 60
end
play 62
