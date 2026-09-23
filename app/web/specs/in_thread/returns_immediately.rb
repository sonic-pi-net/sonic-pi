# in_thread returns to the parent without running the block first
x = 1
in_thread do
  x = 2
  sleep 0.1
end
puts x
sleep 0.2
puts x
