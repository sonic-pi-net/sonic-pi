require 'narray'

def test a
  print a," #=> "
  p eval(a)
  print "\n"
end

test "NArray.float(5).random(10)"

test "NArray.float(5).random"

test "NArray.int(5).random(10)"

test "NArray.int(1000).random(10)"

a = NArray.int(1000).random(10)

idx = (a.eq 0).where
print "a.eq 0  :: n=", idx.size, "\n"

idx = (a.eq 10).where
print "a.eq 10 :: "; p idx
