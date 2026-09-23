# an error kills only its own thread; the rest carry on
use_bpm 120   # at twice the tempo: specs/in_thread/error_in_thread.rb
in_thread do
  sleep 0.25
  raise "thread died"
end
in_thread do
  sleep 0.5
  play 60
end
play 62
sleep 0.5
play 64
