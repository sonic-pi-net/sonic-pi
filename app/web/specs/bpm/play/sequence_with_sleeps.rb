# a melody: notes land at the accumulated sleep times
use_bpm 120   # at twice the tempo: specs/play/sequence_with_sleeps.rb
[60, 62, 64, 65, 67].each do |n|
  play n
  sleep 0.2
end
