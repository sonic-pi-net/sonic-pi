require "narray"
include NMath

x = NArray[65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71]
y = NArray[68, 66, 68, 65, 69, 66, 68, 65, 71, 67, 68, 70]

def test str, x, y=nil
  print str," #=> "
  p eval(str)
end

test "x",x,y
test "y",x,y
test "covariance(x,y)",x,y

a = covariance(x,y)

test "x.stddev",x

test "x.sort",x
test "x.median",x
test "(x+y.newrank!(0)).median(0)",x,y
