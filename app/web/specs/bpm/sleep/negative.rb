# a negative sleep moves logical time backwards and warns
use_bpm 120   # at twice the tempo: specs/sleep/negative.rb
play 60
sleep 1
sleep -0.5
play 62
