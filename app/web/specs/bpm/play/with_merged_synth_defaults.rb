# with_merged_synth_defaults merges inside its block only
use_bpm 120   # at twice the tempo: specs/play/with_merged_synth_defaults.rb
use_synth_defaults amp: 0.5
with_merged_synth_defaults release: 3 do
  play 60
end
play 60
