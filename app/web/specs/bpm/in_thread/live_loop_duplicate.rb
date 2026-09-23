# two live_loops with the same name: the running loop takes the new body
use_bpm 120   # at twice the tempo: specs/in_thread/live_loop_duplicate.rb
live_loop :same do
  play 60
  sleep 0.2
  stop if tick >= 1
end
live_loop :same do
  play 72
  sleep 0.2
  stop if tick >= 1
end
