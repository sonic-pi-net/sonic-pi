# choose and tick draw from a seeded stream and a per-thread counter
notes = (ring 60, 62, 64, 65)
4.times do
  play notes.tick
  play [70, 71, 72].choose, amp: 0.2
  sleep 0.1
end
