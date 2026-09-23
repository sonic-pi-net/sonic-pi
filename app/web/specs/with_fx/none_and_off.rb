# :none and on: false run the block with no fx; the block is given a blank node, and with_fx gives back reps
with_fx :none do
  play 60
end
with_fx :level, on: false do |fx|
  puts fx
  play 62
end
puts(with_fx(:none, reps: 2) { play 64 })
