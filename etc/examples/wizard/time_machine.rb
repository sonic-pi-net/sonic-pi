# Coded by Sam Aaron

use_debug false

live_loop :time do
  synth :tb303, release: 8, note: :e1, cutoff: (range 90, 60, -10).tick
  sleep 8
end

live_loop :machine do
  sample :loop_garzul, rate: (knit 1, 3, -1, 1).tick
  sleep 8
end

live_loop :vortex, auto_cue: false do
  use_synth [:pulse, :beep].choose
  sleep 0.125 / 2
  play scale(:e1, :minor_pentatonic).tick, attack: 0.125, release: 0, amp: 2, cutoff: (ring 70, 90, 100, 130).look
  sleep 0.125 / 2
end

live_loop :moon_bass, auto_cue: false do
  sample :bd_haus, amp: 1.5
  sleep 0.5
end
