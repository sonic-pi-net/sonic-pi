# use_transpose shifts every later note; with_transpose only inside the block
use_bpm 120   # at twice the tempo: specs/play/use_transpose.rb
play 60
use_transpose 12
play 60
play :e4
with_transpose -5 do
  play 60
end
play 60
