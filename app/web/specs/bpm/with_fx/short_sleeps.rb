# a sleep of under 0.4s from where the thread is on the wall clock does not sleep
use_bpm 120   # at twice the tempo: specs/with_fx/short_sleeps.rb
with_fx :level, kill_delay: 0.25 do
  sleep 0.25
  sleep 0.25
end
