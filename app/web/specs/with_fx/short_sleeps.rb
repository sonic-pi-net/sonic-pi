# a sleep of under 0.4s from where the thread is on the wall clock does not sleep
with_fx :level, kill_delay: 0.25 do
  sleep 0.25
  sleep 0.25
end
