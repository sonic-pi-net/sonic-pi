# a thread waiting on a sync wakes on the cuer's wall clock
with_fx :level, kill_delay: 0.25 do
  in_thread do
    sync :go
    play 60, release: 0.25
  end
end
sleep 1
cue :go
