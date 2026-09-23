# an inner fx is one of the outer block's sounds: the outer waits for the inner to be freed
use_bpm 120   # at twice the tempo: specs/with_fx/nested_fx.rb
with_fx :level, kill_delay: 0.25 do
  with_fx :echo, decay: 0.5 do
    play 60, release: 0.25
  end
  sleep 0.25
  play 62, release: 0.25
end
