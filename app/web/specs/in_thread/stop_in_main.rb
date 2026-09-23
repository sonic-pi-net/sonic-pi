# stop in the main thread ends it; spawned threads finish on their own
in_thread do
  sleep 0.5
  play 60
end
stop
play 99
