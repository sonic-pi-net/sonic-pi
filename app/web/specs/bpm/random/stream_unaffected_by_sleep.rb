# sleeping does not touch the stream
use_bpm 120   # at twice the tempo: specs/random/stream_unaffected_by_sleep.rb
use_random_seed 5
a = rand
sleep 0.25
b = rand
use_random_seed 5
rand
puts rand == b
