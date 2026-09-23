# a live_loop's seed: opt restarts its stream each time it is (re)started
live_loop :r, seed: 9 do
  puts rand
  sleep 0.1
  stop if tick >= 2
end
