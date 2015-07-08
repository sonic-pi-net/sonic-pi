require 'narray'

a = NArray.float(3,3).indgen!

print "a #=> "; p a
print "a[1, -1] #=> "; p a[1, -1]
print "a[1, 2..0] #=> "; p a[1, 2..0]
print "a[1...2, 0..1] #=> "; p a[1...2, 0..1]  # without rank-reduce
print "a[true, 0..1] #=> "; p a[true, 0..1]
print "a[0..5] #=> "; p a[0..5]

