# many tiny sleeps add up without drift
use_bpm 120   # at twice the tempo: specs/sleep/accumulates_exactly.rb
100.times { sleep 0.01 }
play 60
