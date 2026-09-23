# chord's invert: opt inverts the chord it makes, up or down
play chord(:e3, :minor, invert: 1)
sleep 1
play chord(:e3, :minor, invert: 2)
sleep 1
play chord(:c4, :major7, invert: -1)
