# the tempo at spawn is inherited
use_bpm 120   # at twice the tempo: specs/in_thread/inherits_bpm.rb
use_bpm 240
in_thread do
  sleep 1
  play 60
end
sleep 1
play 62
