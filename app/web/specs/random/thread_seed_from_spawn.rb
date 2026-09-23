# threads spawned in sequence each start from a different point
use_random_seed 3
in_thread { puts rand }
in_thread { puts rand }
in_thread { puts rand }
