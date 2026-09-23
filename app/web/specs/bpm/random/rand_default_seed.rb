# every run starts from seed 0, so the stream is the same each time
use_bpm 120   # at twice the tempo: specs/random/rand_default_seed.rb
puts rand
puts rand
puts rand(10)
puts rand(0.5)
