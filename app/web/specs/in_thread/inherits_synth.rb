# the synth in force at spawn is inherited; later parent changes are not
use_synth :saw
in_thread do
  play 60
  sleep 0.25
  play 60
end
use_synth :tri
play 62
