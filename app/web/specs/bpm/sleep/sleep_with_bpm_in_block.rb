# nested with_bpm blocks restore the outer tempo afterwards
use_bpm 120   # at twice the tempo: specs/sleep/sleep_with_bpm_in_block.rb
with_bpm 240 do
  sleep 1
  with_bpm 480 do
    sleep 1
    play 60
  end
  sleep 1
  play 62
end
sleep 1
play 64
