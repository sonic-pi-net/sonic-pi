# an fx's own tail (echo's decay) is tempo-scaled; the kill_delay opt is not
use_bpm 120
with_fx :echo, decay: 1 do
  play 60, release: 0.5
end
with_fx :level, kill_delay: 1 do
  play 62, release: 0.5
end
