# format and %: every kind of field
puts format("%d %5d %-5d| %05d %+d %x %o %b %e %g %10.3f", 1, 2, 3, 4, 5, 255, 8, 5, 1234.5, 0.0001, 3.14159)
puts "%s and %p, %c%c %%" % ["text", :sym, 65, "z"]
puts "%-10s|%10s|" % ["left", "right"], "%*d" % [6, 42], "%.3s" % "abcdef"
puts format("%<a>d %{b}", a: 1, b: "two"), "%x" % (2**70), "%.20f" % 0.1
play format("%d", 60).to_i
