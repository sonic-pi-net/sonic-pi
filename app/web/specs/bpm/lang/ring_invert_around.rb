# a ring of notes turned upside down around a note, rests passing through; and a ring's notes
use_bpm 120   # at twice the tempo: specs/lang/ring_invert_around.rb
play (ring :c4, :e4, :g4).invert_around(:c4)
sleep 1
play (ring :c4, :r, :e4).invert_around(:e4)
puts (ring :c4, :r, :e4).notes
puts (ring :c4, :e4, :g4).invert_around(:c4)
