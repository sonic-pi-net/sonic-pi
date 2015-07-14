require "narray"

a = NArray.byte(10)
a[2..4] = 1
p a.class, a.count_true, a.count_false

begin
   a = NArray.float(10)
   a[2..4] = 1
   p a.class, a.count_true, a.count_false
rescue
   print a.class," -- Exception raised as expected. The message was: ", $!,"\n"
end

#-------------------
print "\n--- test masking (float) ---\n"

a = NArray.float(5,3).indgen!
b = (a-2)*2
c = a.lt(b)
p c, c.typecode

p a, b, a.mask( c ), a[c]

#a[c] = ( NArray.int(c.length).indgen!+100 )
#p a

a[c] = 10000
p a

#-------------------
print "\n--- test masking (complex) ---\n"
p a = NArray.complex(5).indgen! + Complex::I

m = NArray.byte(5)
m[true] = [0,0,1,1,0]

p a[m]
a[m] = 100.0
p a
