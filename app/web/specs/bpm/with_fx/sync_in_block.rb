# a thread waiting on a sync wakes on the cuer's wall clock
use_bpm 120   # at twice the tempo: specs/with_fx/sync_in_block.rb
with_fx :level, kill_delay: 0.25 do
  in_thread do
    sync :go
    play 60, release: 0.25
  end
end
sleep 1
cue :go
