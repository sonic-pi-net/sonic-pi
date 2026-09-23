# reps run the block again inside the one fx, which the block can control
use_bpm 120   # at twice the tempo: specs/with_fx/reps_and_control.rb
with_fx :level, reps: 2, kill_delay: 0.25 do |fx|
  play 60, release: 0.25
  sleep 0.5
  control fx, amp: 0.5
end
play 72
