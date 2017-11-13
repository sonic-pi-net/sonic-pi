require 'narray'

a = NArray.new(NArray::DFLOAT,5,5).indgen!
print "a #=> "; p a

idx = [2,0,1]
print "\nidx #=> "
 p idx

print "\na[idx,idx]  # Index Array\n #=> "
 p a[idx,idx]

idx = NArray.to_na(idx)
idx += 10
print "\nidx #=> "
 p idx

print "\na[idx] #=> "
 p a[idx]

print "\na[1,[]]  # Empty Array\n #=> "
 p a[1,[]]

print "\nFollowing will fail...\n\n"
print "a[idx,0] #=> \n"
p a[idx,0]
