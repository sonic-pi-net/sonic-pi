# a rest inside a list of notes stays a rest, and a rest plus a number is still a rest
play [:c4, :r, :g4]
sleep 0.5
play_chord [60, nil, 67]
sleep 0.5
play (ring 60, nil).tick + 12
sleep 0.5
play (ring 60, nil).tick + 12
puts (nil + 3).inspect
