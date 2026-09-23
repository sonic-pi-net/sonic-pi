# horizon: 3.02
# a with_fx in a live_loop's body is freed each time round
use_bpm 120   # at twice the tempo: specs/with_fx/inside_live_loop.rb
live_loop :a do
  with_fx :level, kill_delay: 0.25 do
    play 60, release: 0.25
  end
  sleep 1
end
