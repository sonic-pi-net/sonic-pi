# horizon: 2.02
# a live_loop run again inside the with_fx it is already in carries on there with its new body
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 60, release: 0.1
    sleep 1
  end
  sleep 0.5
  live_loop :a do
    play 62, release: 0.1
    sleep 1
  end
end
