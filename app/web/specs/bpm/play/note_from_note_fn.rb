# note() turns names into numbers before play sees them
use_bpm 120   # at twice the tempo: specs/play/note_from_note_fn.rb
play note(:e4)
play note("a3")
play note(:c4) + 7
