# shuffle on a string and on a symbol, as the Sonic Dreams example does: the characters shuffled, a symbol coming back a symbol
use_bpm 120   # at twice the tempo: specs/random/shuffle_string_symbol.rb
use_random_seed 3
puts shuffle("within")
puts shuffle(:within_dreams)
puts :within_dreams.shuffle
puts "dreams_" + shuffle("within")
puts choose([shuffle(:within_dreams), :within_dreams, :dreams_within])
