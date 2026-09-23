# tick used inside an expression and as an index
notes = [60, 62, 64, 65, 67]
3.times do
  play notes[tick % notes.size] + 12
  sleep 0.1
end
puts look
