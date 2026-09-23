# an error in the main thread does not stop running threads
in_thread do
  sleep 0.5
  play 60
end
play 62
raise "main died"
play 99
