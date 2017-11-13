require 'narray'

def test a,b
  print a," #=> "
  p b
end

a = NArray.int(3,3).indgen!

test "a",a
test "a+1.5",a+1.5
test "1.5+a",1.5+a
test "a+NArray[1.2,3.4,5.6]",a+NArray[1.2,3.4,5.6]
test "a+NArray[Complex(0.5,1.5)]",a+NArray[Complex(0.5,1.5)]
