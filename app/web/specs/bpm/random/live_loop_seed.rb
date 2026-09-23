# a live_loop's seed: opt restarts its stream each time it is (re)started
use_bpm 120   # at twice the tempo: specs/random/live_loop_seed.rb
live_loop :r, seed: 9 do
  puts rand
  sleep 0.1
  stop if tick >= 2
end
