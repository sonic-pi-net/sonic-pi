# a thread starts at the parent's current time and runs alongside it
use_bpm 120   # at twice the tempo: specs/in_thread/basic.rb
in_thread do
  play 60
  sleep 0.5
  play 62
end
play 72
sleep 0.5
play 74
