# use_debug false silences the synth and sample echoes; with_debug scopes it
use_bpm 120   # at twice the tempo: specs/lang/use_debug.rb
use_debug false
play 60
sample :elec_blip
sleep 0.25
use_debug true
play 62
with_debug false do
  play 64
end
play 65
