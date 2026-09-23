# link waits for the next quantum boundary, plus a phase, on the shared timeline
use_bpm 120   # at twice the tempo: specs/lang/link.rb
play 60
sleep 0.3
link
play 62
sleep 0.1
link 2, 0.5
play 64
puts current_beat
