# plain Ruby def works too, and is shared the same way
use_bpm 120   # at twice the tempo: specs/define/ruby_def.rb
def thrice(n) = n * 3
puts thrice(4)
in_thread do
  puts thrice(5)
end
