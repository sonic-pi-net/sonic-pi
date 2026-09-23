# a thread starts at the parent's current time and runs alongside it
in_thread do
  play 60
  sleep 0.5
  play 62
end
play 72
sleep 0.5
play 74
