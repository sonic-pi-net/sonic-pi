# with_bpm applies only inside its block
use_bpm 120   # at twice the tempo: specs/sleep/with_bpm.rb
with_bpm 240 do
  play 60
  sleep 1
end
play 62
sleep 1
play 64
