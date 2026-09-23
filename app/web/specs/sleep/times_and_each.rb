# ordinary Ruby iteration interleaves with sleeps as expected
3.times do |i|
  play 60 + i
  sleep 0.25
end
[70, 72].each_with_index do |n, i|
  play n
  sleep 0.5 * (i + 1)
end
play 80
