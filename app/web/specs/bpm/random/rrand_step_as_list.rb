# rrand's step: can be given as a list (:step, 2) as well as a map, and the numbers land on the step either way
use_bpm 120   # at twice the tempo: specs/random/rrand_step_as_list.rb
use_random_seed 3
play rrand(60, 72, :step, 2)
play rrand(60, 72, step: 2)
play rrand(60, 72, :step, 0.5)
puts rrand(1, 10, :step, 3)
