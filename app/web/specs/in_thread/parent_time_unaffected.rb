# spawning a thread does not advance the parent's time
in_thread do
  sleep 1
end
play 60
