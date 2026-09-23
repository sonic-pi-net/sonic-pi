# blocks close over the parent's variables
count = 0
in_thread do
  count += 1
end
sleep 0.2
puts count
