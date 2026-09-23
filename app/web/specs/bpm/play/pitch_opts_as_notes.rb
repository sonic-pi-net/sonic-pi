# an opt that is a pitch takes a note name as play does: cutoff: :e5, an fx's lpf:, and a control's cutoff:
use_bpm 120   # at twice the tempo: specs/play/pitch_opts_as_notes.rb
s = play 60, cutoff: :e5, release: 2
sleep 0.5
control s, cutoff: :c4
with_fx :rlpf, cutoff: :g4 do
  play 64, cutoff: :a3
end
synth :tb303, note: :c2, cutoff: :c6
