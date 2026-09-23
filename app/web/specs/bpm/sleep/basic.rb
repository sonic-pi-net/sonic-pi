# sleep advances logical time by beats; at 60 bpm a beat is a second
use_bpm 120   # at twice the tempo: specs/sleep/basic.rb
play 60
sleep 1
play 62
sleep 0.5
play 64
