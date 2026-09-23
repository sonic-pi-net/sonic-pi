# a chord named by a string ('7', 'M7', 'm7-5', '+5') is the chord of that name, as a symbol one is
use_bpm 120   # at twice the tempo: specs/lang/chord_names_as_strings.rb
play chord(:c4, '7')
sleep 1
play chord(:c4, 'M7')
sleep 1
play chord(:c4, 'm7-5')
sleep 1
play chord(:c4, '+5')
sleep 1
play chord(:e3, '13')
