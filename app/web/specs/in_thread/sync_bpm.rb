# sync_bpm adopts the cueing thread's tempo
in_thread do
  use_bpm 120
  sleep 1
  cue :now
end
sync_bpm :now
sleep 1
play 60
puts current_bpm
