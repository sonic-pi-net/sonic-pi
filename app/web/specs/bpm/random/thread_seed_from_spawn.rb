# threads spawned in sequence each start from a different point
use_bpm 120   # at twice the tempo: specs/random/thread_seed_from_spawn.rb
use_random_seed 3
in_thread { puts rand }
in_thread { puts rand }
in_thread { puts rand }
