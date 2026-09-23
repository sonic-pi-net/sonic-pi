# at starts threads, which the fx waits for
use_bpm 120   # at twice the tempo: specs/with_fx/at_in_block.rb
with_fx :level, kill_delay: 0.25 do
  at [0.5, 1] do
    play 60, release: 0.25
  end
end
