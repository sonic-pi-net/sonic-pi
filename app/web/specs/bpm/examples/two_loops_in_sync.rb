# the app's example "Two loops in sync" (web/examples.js), recorded up to its horizon
# horizon: 2.001
use_bpm 120   # at twice the tempo: specs/examples/two_loops_in_sync.rb
use_bpm 240
use_synth :saw

live_loop :melody do
  play choose([:e3, :g3, :b3, :d4]), amp: 0.5, release: 0.4, cutoff: 90
  sleep 0.5
end

live_loop :bass do
  sync :melody
  use_synth :tb303
  play :e2, release: 0.9, cutoff: 70, res: 0.3
  sleep 1
end
