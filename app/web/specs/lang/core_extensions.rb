# what native gives every object and number: a ring of anything, ticked; one-argument max, min and clamp; a ring's
# find_index and include?; note_info's to_h; and note_range with pitches:, which asks a ring what it includes
puts (1..4).ring
puts "x--x".tick
puts :ab.tick
puts 5.max(3)
puts 5.min(8)
puts 12.clamp(10)
puts -12.clamp(10)
puts (ring 60, 64, 67).find_index(64)
puts (ring 60, 64, 67).include?(64)
puts note_info(:e4).to_h[:midi_note]
play note_range(:c4, :c5, pitches: (chord :c, :major))
