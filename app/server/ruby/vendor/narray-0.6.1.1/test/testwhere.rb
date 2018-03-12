require 'narray'

a = NArray.float(8).indgen!

print "a = "
p a

t,f = ( (a>=5).or(a<2) ).where2
print "t,f = ( (a>=5).or (a<2) ).where2\n"
print "t = "; p t
print "f = "; p f

t,f = ( (a>=5).and(a<2) ).where2
print "t,f = ( (a>=5).and (a<2) ).where2\n"
print "t = "; p t
print "f = "; p f

print "\n vvv no-meaning !! vvv\n"
t,f = ( (a>=5) && (a<2) ).where2
print "t,f = ( (a>=5) && (a<2) ).where2\n"
print "t = "; p t
print "f = "; p f

t,f = ( (a>=5) || (a<2) ).where2
print "t,f = ( (a>=5) || (a<2) ).where2\n"
print "t = "; p t
print "f = "; p f
