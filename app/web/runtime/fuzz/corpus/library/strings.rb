# the string methods beyond the core
s = "Hello, Sonic Pi World"
puts s.swapcase, s.center(30, "*"), s.ljust(25, ".") + "|", s.rjust(25) , s.squeeze("l"), s.tr("lo", "01"), s.delete("aeiou")
puts s.count("o"), s.index("o", 5), s.rindex("o"), s.partition(", ").inspect, s.rpartition(" ").inspect, s.each_char.to_a.size
puts s.start_with?("Hell"), s.end_with?("d"), s.succ, s.ord, s.hex, s.oct, "0x1f".hex, s.casecmp("HELLO, sonic pi world")
puts s.scan(/[A-Z]\w+/).inspect, s.gsub(/o/) { |m| m.upcase }, s.sub(/(\w+), (\w+)/, '\2 \1'), s =~ /Pi/, s[/S(on)ic/, 1]
puts s.bytes.take(3).inspect, s.chars.last, s.lines.size, s.slice(7, 5), s.insert(5, "!").size, s.reverse, s * 2, s.unpack("a5")
puts s.encoding rescue puts "no encoding"
puts "%s" % s.freeze.frozen?, s.dup.concat(" x", " y").chomp(" y").chop.strip.lstrip.rstrip.upcase.downcase.capitalize rescue puts "no"
play s.size + 40
