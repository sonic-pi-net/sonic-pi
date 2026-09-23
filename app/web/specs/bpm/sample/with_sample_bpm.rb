# with_sample_bpm scopes the tempo change
use_bpm 120   # at twice the tempo: specs/sample/with_sample_bpm.rb
with_sample_bpm :loop_amen do
  sleep 1
  play 60
end
sleep 1
play 62
