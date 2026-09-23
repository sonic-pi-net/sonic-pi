# loop runs until stop; stop ends the current thread only
use_bpm 120   # at twice the tempo: specs/sleep/loop_with_stop.rb
loop do
  play 60
  sleep 0.25
  stop if current_beat >= 0.75
end
play 99
