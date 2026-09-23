# nested with_bpm blocks restore the outer tempo afterwards
with_bpm 120 do
  sleep 1
  with_bpm 240 do
    sleep 1
    play 60
  end
  sleep 1
  play 62
end
sleep 1
play 64
