# several threads interleave on their own timelines
in_thread do
  3.times { play 60; sleep 0.3 }
end
in_thread do
  2.times { play 72; sleep 0.45 }
end
play 48
