# chord_invert, chord_degree, degree, note_range, note_info, rest?, scale_names and chord_names
puts chord_invert(chord(:a3, :major), 1)
puts chord_invert(chord(:a3, :major), -2)
puts chord_degree(:iv, :c4, :major)
puts chord_degree(2, :e3, :minor, 3, invert: 1)
puts degree(:iii, :d4, :major)
puts degree(2, :c4, :minor)
puts degree(:a5, :c4, :major)
puts degree(9, :c4, :major)
puts note_range(:c4, :c5)
puts note_range(:c5, :c4, pitches: [:c, :e, :g])
puts note_info(:c4)
puts note_info(61)
puts note_info(:eb, octave: 2)
puts rest?(:r), rest?(60), rest?(nil), rest?({note: :rest})
puts scale_names.size, scale_names[0], scale_names[-1]
puts chord_names.size, chord_names[0], chord_names[-1]
