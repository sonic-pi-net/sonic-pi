# with_synth applies only inside its block
use_bpm 120   # at twice the tempo: specs/play/with_synth.rb
use_synth :tb303
with_synth :pretty_bell do
  play 60
end
play 60
