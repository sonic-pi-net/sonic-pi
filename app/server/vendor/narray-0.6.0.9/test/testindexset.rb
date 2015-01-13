require 'narray'

a = NArray.float(3,3).indgen!

print "a #=> "; p a

print "\na[[]] = []  # Do nothing \na #=> ";
a[[]] = []; p a

print "\na[1,1] = 0  # 1-element replace\na #=> ";
b=a.dup; b[1,1] = 0
p b

print "\na[0..-1, 1] = 0  # Range replace\na #=> ";
b=a.dup; b[0..-1, 1] = 0
p b

print "\na[1,2..0] = [100,101,102]  # Array replace\na #=> ";
b=a.dup; b[1,2..0] = [100,101,102]
p b

print "\na[0,1] = [100,101,102]  # Specifing Starting point \na #=> ";
b=a.dup; b[0,1] = [100,101,102]
p b

print "\na[1,0] = [[100],[101],[102]]  # Specifing Starting point \na #=> ";
b=a.dup; b[1,0] = [[100],[101],[102]]
p b

print "\na[true,1] = [100,101,102]  # `true' means entire range\na #=> ";
b=a.dup; b[true,1] = [100,101,102]
p b

print "\na[true,true] = [[100,101,102]] \na #=> ";
b=a.dup; b[true,true] = [[100,101,102]]
p b


print "\nFollowing will fail ...\n"

print "\na[true,1] = [[100,101,102]] \na #=> ";
b=a.dup; b[true,1] = [[100,101,102]]
p b

print "\na[true,1] = [[100],[101],[102]] \na #=> ";
b=a.dup; b[true,1] = [[100],[101],[102]]
p b

print "\na[true,true] = [100,101,102] \na #=> ";
b=a.dup; b[true,true] = [100,101,102]
p b

print "\na[1,0] = [100,101,102] \na #=> ";
b=a.dup; b[1,0] = [100,101,102]
p b
