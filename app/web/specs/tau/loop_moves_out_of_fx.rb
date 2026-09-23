# expected: by hand
# horizon: 3.02
# Tau, not Sonic Pi: a live_loop run again from outside leaves its with_fx, which is then freed
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 60, release: 0.1
    sleep 1
  end
end
sleep 1.5
live_loop :a do
  play 62, release: 0.1
  sleep 1
end
