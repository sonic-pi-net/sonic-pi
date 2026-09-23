# a bpm set in the parent after spawning does not reach the thread
use_bpm 120   # at twice the tempo: specs/sleep/bpm_is_thread_local.rb
use_bpm 120
in_thread do
  sleep 1
  play 60
end
use_bpm 240
sleep 1
play 62
