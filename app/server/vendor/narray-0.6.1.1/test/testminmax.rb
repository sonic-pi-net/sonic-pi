require 'narray'

def test a
  print "a = "
  p a
  print "a.min = "
  p a.min
  print "a.max = "
  p a.max
  print "a.sum = "
  p a.sum
  print "a.mean = "
  p a.mean
  print "a.stddev = "
  p a.stddev
end

a = NArray[0,2,-2.5,3,1.4]

test a

a = NArray.float(5,1).indgen!(1)
b = NArray.float(1,3).indgen!(1)
a *= b
test a

print "a.min(0) = "
p a.min(0)
print "a.max(0) = "
p a.max(0)
print "a.min(1) = "
p a.min(1)
print "a.max(1) = "
p a.max(1)
print "a.sum(0) = "
p a.sum(0)
print "a.sum(1) = "
p a.sum(1)
print "a.mean(0) = "
p a.mean(0)
print "a.mean(1) = "
p a.mean(1)
print "a.stddev(0) = "
p a.stddev(0)
print "a.stddev(1) = "
p a.stddev(1)
