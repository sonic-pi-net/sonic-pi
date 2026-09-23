# the app's example "Define and threads" (web/examples.js), recorded up to its horizon
# horizon: 2.001
use_bpm 120   # at twice the tempo: specs/examples/define_and_threads.rb
use_bpm 180

define :phrase do |root|
  play_pattern_timed scale(root, :minor_pentatonic).take(5), 0.25, release: 0.2
end

in_thread do
  use_synth :piano
  phrase :e3
  sleep 1
  phrase :a3
end

in_thread do
  use_synth :dark_ambience
  play :e2, sustain: 4, release: 2, amp: 0.5
end
