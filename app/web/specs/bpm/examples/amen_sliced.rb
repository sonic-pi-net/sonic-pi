# the app's example "Amen, sliced" (web/examples.js), recorded up to its horizon
# horizon: 2.001
use_bpm 120   # at twice the tempo: specs/examples/amen_sliced.rb
use_bpm 260

live_loop :amen do
  sample :loop_amen, onset: [0, 1, 2, 3, 4, 5, 6, 7].tick, amp: 0.8
  sleep 0.125
end

live_loop :kick do
  sample :bd_haus, lpf: 70
  sleep 0.5
end
