# a note name does arithmetic as its MIDI number: plus or minus a number or another note; a rest stays a rest
play :e3 + 4
play :c4 - 12
play :e + 2
play 60 + (:e4 - :c4)
play :fs3 + 0.5
play :r + 1
play :eb3.to_f
play :a4.to_i + 1
