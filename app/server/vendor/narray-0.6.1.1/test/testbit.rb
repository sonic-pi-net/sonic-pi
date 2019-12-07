require "narray"

a = NArray.byte(10).indgen!

def test a, str
  print str," #=>\n"
  p eval(str)
end

test a, "a"
test a, "a & 1"
test a, "a & 2"
test a, "a & -1"

test a, "a | 1"
test a, "a | 2"
test a, "a | -1"

test a, "a ^ 1"
test a, "a ^ 2"
test a, "a ^ -1"

test a, "~a"

a = NArray.int(10).indgen!
test a, "a"
test a, "~a"
