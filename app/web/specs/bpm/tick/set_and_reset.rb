# tick_set, tick_reset and tick_reset_all
use_bpm 120   # at twice the tempo: specs/tick/set_and_reset.rb
tick
tick
puts look
tick_set 40
puts look
puts tick
tick(:x)
tick_reset
puts look
puts look(:x)
tick_reset_all
puts look(:x)
