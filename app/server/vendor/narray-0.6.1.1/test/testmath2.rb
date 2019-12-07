require "narray"
include NMath

def testm x,name
  f = "a#{name}(#{name}(x))"
  print "\n### #{f} ###\n"
  y = eval(f)
  p y
  #d = x-y
  #p d.abs
end

PI=Math::PI

i = NArray.complex(1)
i.imag=1

n=7
x = NArray.complex(n).random!(1)
x.imag= NArray.float(n).random!(1)

p x
testm x,"sin"
testm x,"cos"
testm x,"tan"
testm x,"sinh"
testm x,"cosh"
testm x,"tanh"
testm x,"sec"
testm x,"sech"

p cot(1)

exit

a= -i * log( sqrt(1-x**2)*i + x )
b= -i * log( sqrt(x**2-1) + x )
p a
p b
p a-b

a= -i * log( sqrt(1-x**2) + x*i )
b= -i * log( (-sqrt(x**2-1) + x)*i )
p a
p b
p a-b
