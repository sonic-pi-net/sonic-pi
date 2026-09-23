# two blocks in turn: each is freed by its own sounds and where the thread was when it ended
use_bpm 120   # at twice the tempo: specs/with_fx/one_after_another.rb
with_fx :level, kill_delay: 0.5 do
  play 60, release: 0.25
  sleep 1
end
with_fx :level, kill_delay: 0.25 do
  play 62, release: 0.25
end
