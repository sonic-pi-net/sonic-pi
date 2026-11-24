# pack and unpack: numbers and text to bytes and back
b = [1, 2, 65535, -1, 3.5, 1e10].pack("CnNlef")
puts b.size, b.unpack("CnNlef").inspect
puts ["abc", "de"].pack("A5a3").unpack("A5a3").inspect
puts [1, 2, 3].pack("U*").unpack("U*").inspect
puts ["hello world"].pack("m").unpack("m").inspect
puts ["616263"].pack("H*"), "abc".unpack("H*").inspect, [300, 70000].pack("w*").unpack("w*").inspect
play b.unpack("C*").first + 60
