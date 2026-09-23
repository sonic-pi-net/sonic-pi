# use_tuning and with_tuning retune notes before they reach the synth
use_bpm 120   # at twice the tempo: specs/lang/tuning.rb
use_tuning :just
play :e4
play 64.5
with_tuning :pythagorean, :d do
  play :fs4
end
play :g4
use_tuning :equal
play :e4
