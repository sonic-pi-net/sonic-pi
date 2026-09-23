# every pass of a live_loop cues its own name; another loop can sync on it
live_loop :a do
  sleep 0.3
  stop if tick >= 1
end
live_loop :b, sync: :a do
  play 60
  stop
end
