# sleeping does not touch the stream
use_random_seed 5
a = rand
sleep 0.25
b = rand
use_random_seed 5
rand
puts rand == b
