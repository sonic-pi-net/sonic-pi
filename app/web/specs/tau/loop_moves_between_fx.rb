# expected: by hand
# horizon: 3.02
# Tau, not Sonic Pi: a live_loop moves from one with_fx to another; the first is freed, the second keeps it
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 60, release: 0.1
    sleep 1
  end
end
sleep 1.5
with_fx :echo, decay: 0.5 do
  live_loop :a do
    play 62, release: 0.1
    sleep 1
  end
end
