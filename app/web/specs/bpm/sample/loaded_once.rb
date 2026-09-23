# a sample is loaded once; later plays reuse the buffer
use_bpm 120   # at twice the tempo: specs/sample/loaded_once.rb
sample :loop_amen
sleep 0.25
sample :loop_amen
sample :loop_amen, amp: 0.5
