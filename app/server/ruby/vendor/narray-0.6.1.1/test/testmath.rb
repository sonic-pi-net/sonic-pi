require 'narray'
include NMath

def pr x
  x.each{|i|
    if i.kind_of?(Complex)
      printf("%.3f%+.3fi ",i.real,i.imag)
    else
      printf("%.3f ",i)
    end
  }
  print "\n"
end

def testmath(x)
  print "x = "
  pr x
  print "sqrt(x) = "
  pr sqrt(x)
  print "sin(x) = "
  pr sin(x)
  print "cos(x) = "
  pr cos(x)
  print "tan(x) = "
  pr tan(x)
  print "sinh(x) = "
  pr sinh(x)
  print "cosh(x) = "
  pr cosh(x)
  print "tanh(x) = "
  pr tanh(x)
  print "exp(x) = "
  pr exp(x)
  print "log(x) = "
  pr log(x)
  print "log10(x) = "
  pr log10(x)
  print "atan(x) = "
  pr atan(x)
  print "atan(tan(x)) = "
  pr atan(tan(x))
end

testmath NArray.sfloat(6).indgen.div!(2)
testmath NArray.float(6).indgen.div!(2)

testmath NArray.scomplex(6).indgen.div!(2)-2 - Complex(0,1)
testmath NArray.complex(6).indgen!/5-0.5# - Complex(0,0.3)
