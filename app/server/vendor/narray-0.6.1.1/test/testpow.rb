require "narray"

def test a
  puts 'a = '
  p  a
  puts 'a**[[-3],[0],[7]] = '
  p  a**[[-3],[0],[7]]
  puts 'a**[[-3.0],[0],[7.0]] = '
  p  a**[[-3.0],[0],[7.0]]
  puts 'a**Complex(1,0) = '
  p  a**Complex(1,0)
  puts 'a**1.0 = '
  p  a**1.0
  puts
end

test NArray.int(4).indgen!*2-2
test NArray.float(4).indgen!*2-2
test NArray.complex(4).indgen!*2-2
