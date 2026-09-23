# horizon: 3.02
# a with_fx in a live_loop's body is freed each time round
live_loop :a do
  with_fx :level, kill_delay: 0.25 do
    play 60, release: 0.25
  end
  sleep 1
end
