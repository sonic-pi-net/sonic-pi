# an unknown sample name is skipped with a message, not an error
use_bpm 120   # at twice the tempo: specs/sample/unknown_sample.rb
sample :nonesuch_sample
sleep 0.25
play 60
