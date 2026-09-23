# live_loop is a named thread that loops; stop ends it
live_loop :beat do
  play 36
  sleep 0.25
  stop if current_beat >= 0.75
end
