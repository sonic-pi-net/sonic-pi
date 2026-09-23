# play_chord is the same as play with a list
use_bpm 120   # at twice the tempo: specs/play/play_chord.rb
play_chord [60, 64, 67]
sleep 0.5
play_chord [:c4, :e4, :g4], release: 0.2
