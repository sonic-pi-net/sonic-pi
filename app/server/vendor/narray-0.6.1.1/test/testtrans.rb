require 'narray'

def test a,b
  print a," #=> "
  p b
end

a = NArray.int(4,3,2).indgen!
test "a",a

print "\n# transpose first and second...\n"
test "a.transpose(1,0)",a.transpose(1,0)

print "\n# transpose first and last...\n"
test "a.transpose(-1,1..-2,0)",a.transpose(-1,1..-2,0)

print "\n# transpose shift forward ...\n"
test "a.transpose(1..-1,0)",a.transpose(1..-1,0)
