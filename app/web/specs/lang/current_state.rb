# the current_* getters, beat, rt, bt, vt, block_duration and block_slept?
puts current_transpose, current_octave, current_cent_tuning
use_transpose 3
use_octave -1
use_cent_tuning 10
puts current_transpose, current_octave, current_cent_tuning
puts current_debug
use_debug false
puts current_debug
use_random_seed 42
rand
rand
puts current_random_seed
puts current_random_source
use_random_source :pink
puts current_random_source
use_bpm 120
puts current_beat_duration
puts rt(1), bt(1)
sleep 1
puts beat, vt
puts block_duration { sleep 2 }
puts block_slept? { play 60 }
puts block_slept? { sleep 0.5 }
