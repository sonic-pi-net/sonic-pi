# a range steps as Ruby's does: integers, a float step without drift, an exclusive end, and opts given as a list
(0..6).step(2) { |n| play 60 + n }
sleep 0.5
(0..1).step(0.25).each { |x| play 60, amp: x }
sleep 0.5
puts (0...3).step(1).to_a
play 62, *[:amp, 0.5, :pan, -1]
