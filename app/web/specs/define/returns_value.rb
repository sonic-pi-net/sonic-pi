# the value of the last expression is returned, including nil and rings
define :notes do
  (ring 60, 62, 64)
end
define :nothing do
end
puts notes
puts nothing
puts notes[1]
