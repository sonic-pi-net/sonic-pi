# the value of the last expression is returned, including nil and rings
use_bpm 120   # at twice the tempo: specs/define/returns_value.rb
define :notes do
  (ring 60, 62, 64)
end
define :nothing do
end
puts notes
puts nothing
puts notes[1]
