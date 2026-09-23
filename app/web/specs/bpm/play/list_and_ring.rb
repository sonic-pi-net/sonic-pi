# arrays and rings of notes play together
use_bpm 120   # at twice the tempo: specs/play/list_and_ring.rb
play [60, 64]
sleep 0.5
play ring(62, 65, 69)
sleep 0.5
play scale(:c4, :major_pentatonic)
