# a killed synth ends when the kill reaches the engine
use_bpm 120   # at twice the tempo: specs/with_fx/kill_ends_sooner.rb
with_fx :level, kill_delay: 0.25 do
  s = play 60, release: 5
  sleep 0.5
  kill s
end
