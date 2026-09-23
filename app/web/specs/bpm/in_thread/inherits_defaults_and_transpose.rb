# synth defaults, transpose and octave are all inherited at spawn
use_bpm 120   # at twice the tempo: specs/in_thread/inherits_defaults_and_transpose.rb
use_synth_defaults amp: 0.5
use_transpose 12
use_octave -1
in_thread do
  play 60
end
