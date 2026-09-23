# use_bpm inside a thread does not change the parent's tempo
in_thread do
  use_bpm 240
  sleep 1
  play 60
end
sleep 1
play 62
