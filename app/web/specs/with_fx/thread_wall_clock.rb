# a sleep wakes 0.2s before its time, on the wall clock the fx is freed by
with_fx :level, kill_delay: 0.25 do
  in_thread do
    sleep 1
  end
end
