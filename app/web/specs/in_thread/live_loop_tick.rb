# live_loop with tick; the loop ends after a few passes
live_loop :arp do
  play (ring 60, 64, 67).tick
  sleep 0.2
  stop if look >= 4
end
