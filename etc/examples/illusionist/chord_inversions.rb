# Coded by Adrian Cheater

# (in a single tweet)
# https://twitter.com/wpgFactoid/status/666692596605976576

[1, 3, 6, 4].each do |d|
  (range -3, 3).each do |i|
    play_chord (chord_degree d, :c, :major, 3, invert: i)
    sleep 0.25
  end
end
