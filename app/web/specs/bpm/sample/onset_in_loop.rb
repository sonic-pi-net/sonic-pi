# stepping through onsets with tick
use_bpm 120   # at twice the tempo: specs/sample/onset_in_loop.rb
8.times do
  sample :loop_amen, onset: tick
  sleep 0.1
end
