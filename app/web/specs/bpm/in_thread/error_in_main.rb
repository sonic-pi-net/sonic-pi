# an error in the main thread does not stop running threads
use_bpm 120   # at twice the tempo: specs/in_thread/error_in_main.rb
in_thread do
  sleep 0.5
  play 60
end
play 62
raise "main died"
play 99
