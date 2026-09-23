# a thread spawned after a sleep starts at that later time
use_bpm 120   # at twice the tempo: specs/in_thread/starts_at_spawn_time.rb
sleep 0.5
in_thread do
  play 60
  sleep 0.25
  play 62
end
