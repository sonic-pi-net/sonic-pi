# logical time is unaffected by set_sched_ahead_time!
use_bpm 120   # at twice the tempo: specs/sleep/sched_ahead_independent.rb
set_sched_ahead_time! 1
play 60
sleep 0.5
play 62
