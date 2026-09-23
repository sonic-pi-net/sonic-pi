# reps run the block again inside the one fx, which the block can control
with_fx :level, reps: 2, kill_delay: 0.25 do |fx|
  play 60, release: 0.25
  sleep 0.5
  control fx, amp: 0.5
end
play 72
