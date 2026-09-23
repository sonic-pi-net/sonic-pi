# use_bpm inside a thread does not change the parent's tempo
use_bpm 120   # at twice the tempo: specs/sleep/use_bpm_returns.rb
in_thread do
  use_bpm 480
  sleep 1
  play 60
end
sleep 1
play 62
