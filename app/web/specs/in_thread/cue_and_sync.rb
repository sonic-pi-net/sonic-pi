# sync waits for a cue and takes its time
in_thread do
  sleep 0.5
  cue :tick
end
sync :tick
play 60
