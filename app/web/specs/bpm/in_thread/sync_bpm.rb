# sync_bpm adopts the cueing thread's tempo
use_bpm 120   # at twice the tempo: specs/in_thread/sync_bpm.rb
in_thread do
  use_bpm 240
  sleep 1
  cue :now
end
sync_bpm :now
sleep 1
play 60
puts current_bpm
