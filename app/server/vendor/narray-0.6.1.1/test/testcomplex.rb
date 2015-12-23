require 'narray'

def testop(a,b)
  print "a = "; p a
  print "b = "; p b
  print "a+b = "; p a+b
  print "a-b = "; p a-b
  print "a*b = "; p a*b
  print "a/b = "; p a/b
  print "a**b = "; p a**b
end

a = NArray.complex(4,1).indgen!.sbt!(-1) + Complex(0,0.25)
b = NArray.complex(1,3).indgen!.add!(-0.5).mul!(Complex(0,0.5))
testop(a,b)

# compare a/b with real-number operation
# a = NArray(4,1).indgen!.sbt!(-1)
# b = NArray(1,3).fill!(1)
# c = b*b + b*b
# p ( (a*b + 0*b)/c )
# p ( (0*b - a*b)/c )

def testimag(a)
  print "a.real = "
  p a.real
  print "a.imag = "
  p a.imag
  print "a.angle = "
  p a.angle
  print "a.conj = "
  p a.conj
end

testimag a
