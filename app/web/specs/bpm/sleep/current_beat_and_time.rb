# current_beat and the logical clock move together
use_bpm 120   # at twice the tempo: specs/sleep/current_beat_and_time.rb
puts current_beat
sleep 0.5
puts current_beat
use_bpm 240
sleep 1
puts current_beat
puts current_bpm
