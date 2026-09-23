# live_loop with tick; the loop ends after a few passes
use_bpm 120   # at twice the tempo: specs/in_thread/live_loop_tick.rb
live_loop :arp do
  play (ring 60, 64, 67).tick
  sleep 0.2
  stop if look >= 4
end
