# horizon: 2.02
# a live_loop in a with_fx keeps it for as long as it runs, through its scope's fx
use_bpm 120   # at twice the tempo: specs/with_fx/live_loop_inside.rb
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 60, release: 0.1
    sleep 1
  end
end
