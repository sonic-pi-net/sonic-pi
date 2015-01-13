require 'narray'

def testround a
  print "a = ";  p a
  print "a.floor = ";  p a.floor
  print "a.ceil  = ";  p a.ceil
  print "a.round = ";  p a.round
  print "\n"
end

testround NArray.float(4,2).indgen!/4-2
