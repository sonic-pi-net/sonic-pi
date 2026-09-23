# a sleep wakes 0.2s before its time, on the wall clock the fx is freed by
use_bpm 120   # at twice the tempo: specs/with_fx/thread_wall_clock.rb
with_fx :level, kill_delay: 0.25 do
  in_thread do
    sleep 1
  end
end
