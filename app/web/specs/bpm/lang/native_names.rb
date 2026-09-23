# the names native keeps: invert_chord, live_state, with_afx, the timing warnings pair, the midi ports
use_bpm 120   # at twice the tempo: specs/lang/native_names.rb
play invert_chord((chord :c4, :major), 1)
set :x, 3
puts live_state(:x)
with_afx :reverb do
  play 70
  sleep 1
end
play 72
use_timing_warnings false
with_timing_warnings true do
  play 74
end
puts midi_available_ports
puts use_midi_ports("*")
puts current_midi_ports
