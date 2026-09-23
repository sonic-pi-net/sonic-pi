# a thread spawned after a sleep starts at that later time
sleep 0.5
in_thread do
  play 60
  sleep 0.25
  play 62
end
