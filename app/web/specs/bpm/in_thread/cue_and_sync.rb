# sync waits for a cue and takes its time
use_bpm 120   # at twice the tempo: specs/in_thread/cue_and_sync.rb
in_thread do
  sleep 0.5
  cue :tick
end
sync :tick
play 60
