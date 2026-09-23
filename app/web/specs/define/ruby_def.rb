# plain Ruby def works too, and is shared the same way
def thrice(n) = n * 3
puts thrice(4)
in_thread do
  puts thrice(5)
end
