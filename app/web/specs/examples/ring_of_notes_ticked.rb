# the app's example "Ring of notes, ticked" (web/examples.js), recorded up to its horizon
# horizon: 2.001
use_bpm 100
use_synth :pluck

live_loop :arp do
  play [:c4, :e4, :g4, :b4, :c5, :b4, :g4, :e4].tick, amp: 0.5, release: 0.3
  sleep 0.25
end

live_loop :drone do
  use_synth :organ_tonewheel
  play :c3, attack: 2, sustain: 2, release: 2, amp: 0.2
  sleep 6
end
