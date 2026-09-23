# note's octave: opt sets the octave outright, whatever the name or number had: note(:a, octave: 1) is A1
use_bpm 120   # at twice the tempo: specs/play/note_octave_opt.rb
use_synth :mod_saw
play note(:a, octave: 1)
play note(:a3, octave: 5)
play note(60, octave: 2)
play note(:a)
