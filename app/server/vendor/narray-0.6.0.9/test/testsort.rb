require 'narray'

def test a,b
  print a," #=> "
  p b
end

a = NArray.int(16).random!(100)

test "a",a
test "a.sort",a.sort

print "\n# as string...\n"
a = a.to_string
test "a",a
test "a.sort",a.sort

print "\n# up to 0-rank...\n"
a = NArray.int(4,4).random!(100)
test "a",a
test "a.sort(0)",a.sort(0)

print "\n# big array test...\n"
a = NArray.int(10,10,10,10,10).random!(100)
test "a",a
test "a.sort(1)",a.sort(1)

print "\n# test sort_index...\n"
a = NArray.int(7,4).random!(100)
test "a",a
test "a.sort_index(0)",idx=a.sort_index(0)
test "a[a.sort_index]",a[idx]

print "\n# following will fail...\n"
a = NArray.complex(3,3).random!(100)
test "a",a
test "a.sort",a.sort
