require 'narray'

a = NArray.float(3,3)
p a.indgen!
p a.to_string
p NArray.complex(3,3).indgen!.div!(3).to_string
p NArray.scomplex(3,3).indgen!.div!(3).to_string
p NArray.byte(3,3).indgen!.add!(32).to_string

a = NArray.int(3,3)
p a.indgen!
p a.to_string
p NArray.sfloat(3,3).indgen!.to_string.to_string
