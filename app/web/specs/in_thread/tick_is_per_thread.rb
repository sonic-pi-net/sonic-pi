# tick counters are per thread
notes = (ring 60, 62, 64, 66)
in_thread do
  3.times do
    play notes.tick
    sleep 0.1
  end
end
2.times do
  play notes.tick + 12
  sleep 0.1
end
