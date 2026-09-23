# threads started in the block, and theirs, keep the fx until they and their sounds end
use_bpm 120   # at twice the tempo: specs/with_fx/threads_that_sleep.rb
with_fx :level, kill_delay: 0.5 do
  in_thread do
    sleep 0.5
    in_thread do
      sleep 0.5
      play 60, release: 0.25
    end
  end
  play 50, release: 0.25
end
sleep 0.25
play 70
