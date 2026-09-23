# each thread keeps its own logical clock
in_thread do
  sleep 0.3
  play 60
  sleep 0.3
  play 62
end
sleep 0.2
play 70
sleep 0.2
play 72
