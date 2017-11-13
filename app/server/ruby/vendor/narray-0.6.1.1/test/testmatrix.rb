require "narray"

m = NMatrix.float(3,3,3).indgen!

puts
puts 'm  #=>'
p m			#=> NMatrix.float(3,3,3):
puts
puts 'm[1,1,true]  #=>'
p m[1,1,true]		#=> NArray.float(3): 
puts
puts 'm[0..1,2,true]  #=>'
p m[0..1,2,true]	#=> NMatrix.float(2,1,3): 
