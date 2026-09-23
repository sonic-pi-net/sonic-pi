# use_bpm_mul scales the current tempo
use_bpm 60
use_bpm_mul 2
play 60
sleep 1
play 62
with_bpm_mul 0.5 do
  sleep 1
  play 64
end
sleep 1
play 65
