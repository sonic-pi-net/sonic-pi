# named counters are independent of each other and of the default
puts tick
puts tick(:a)
puts tick(:a)
puts tick
puts look(:a)
puts tick(:b, step: 2)
puts look(:b)
