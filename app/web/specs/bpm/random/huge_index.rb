# the table wraps: seeds and skips past its length keep working
use_bpm 120   # at twice the tempo: specs/random/huge_index.rb
use_random_seed 1_000_000
puts rand
rand_skip 500_000
puts rand
