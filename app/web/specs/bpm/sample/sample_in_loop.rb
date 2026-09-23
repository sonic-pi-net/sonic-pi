# a drum pattern
use_bpm 120   # at twice the tempo: specs/sample/sample_in_loop.rb
4.times do |i|
  sample :drum_heavy_kick
  sample :drum_cymbal_closed if i.odd?
  sleep 0.25
end
