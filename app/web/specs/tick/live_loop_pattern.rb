# the idiom: a live_loop stepping through a ring with tick
live_loop :arp do
  play (ring 60, 63, 67, 70).tick
  sleep 0.125
  stop if look >= 5
end
