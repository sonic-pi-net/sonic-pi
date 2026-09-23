# density repeats the block at a faster tempo, inside the fx
use_bpm 120   # at twice the tempo: specs/with_fx/density_in_block.rb
with_fx :level, kill_delay: 0.25 do
  density 2 do
    play 60, release: 0.5
    sleep 0.5
  end
end
