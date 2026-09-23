# a bpm set in the parent after spawning does not reach the thread
use_bpm 60
in_thread do
  sleep 1
  play 60
end
use_bpm 120
sleep 1
play 62
