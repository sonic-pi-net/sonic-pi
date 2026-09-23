# use_bpm 120: a beat is half a second; beats count as before
use_bpm 120   # at twice the tempo: specs/sleep/use_bpm.rb
use_bpm 240
play 60
sleep 1
play 62
sleep 1
play 64
puts current_bpm
