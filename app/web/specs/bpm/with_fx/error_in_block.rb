# an error in the block still frees the fx
use_bpm 120   # at twice the tempo: specs/with_fx/error_in_block.rb
with_fx :level, kill_delay: 0.25 do
  play 60, release: 0.25
  sleep 0.5
  raise "boom"
end
