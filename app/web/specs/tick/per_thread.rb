# counters are per thread and a child starts fresh
tick
tick
in_thread do
  puts look
  puts tick
end
puts look
