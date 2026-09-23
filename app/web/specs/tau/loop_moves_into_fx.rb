# expected: by hand
# horizon: 3.02
# Tau, not Sonic Pi: a running live_loop run again from inside a with_fx moves into it, and the fx keeps it
live_loop :a do
  play 60, release: 0.1
  sleep 1
end
sleep 1.5
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 62, release: 0.1
    sleep 1
  end
end
