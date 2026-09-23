# every pass of a live_loop cues its own name; another loop can sync on it
use_bpm 120   # at twice the tempo: specs/in_thread/live_loop_auto_cue.rb
live_loop :a do
  sleep 0.3
  stop if tick >= 1
end
live_loop :b, sync: :a do
  play 60
  stop
end
