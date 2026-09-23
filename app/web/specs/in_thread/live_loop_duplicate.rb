# two live_loops with the same name: the running loop takes the new body
live_loop :same do
  play 60
  sleep 0.2
  stop if tick >= 1
end
live_loop :same do
  play 72
  sleep 0.2
  stop if tick >= 1
end
