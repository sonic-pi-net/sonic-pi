# tick on an array or a ring picks the next element, wrapping round
4.times do
  play [60, 64, 67].tick
  sleep 0.1
end
4.times do
  sample :bd_haus, onset: [0, 1, 2].tick
  sleep 0.1
end
