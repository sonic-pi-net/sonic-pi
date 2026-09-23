# tick used inside an expression and as an index
use_bpm 120   # at twice the tempo: specs/tick/tick_in_expression.rb
notes = [60, 62, 64, 65, 67]
3.times do
  play notes[tick % notes.size] + 12
  sleep 0.1
end
puts look
