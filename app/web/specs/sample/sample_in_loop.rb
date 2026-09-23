# a drum pattern
4.times do |i|
  sample :drum_heavy_kick
  sample :drum_cymbal_closed if i.odd?
  sleep 0.25
end
