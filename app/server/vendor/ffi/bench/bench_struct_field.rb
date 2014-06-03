require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

class TestStruct < FFI::Struct
  layout :i, :int, :p, :pointer
end

s = TestStruct.new(FFI::MemoryPointer.new(TestStruct))
puts "Benchmark FFI Struct.get(:int) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; max = iter / 4; while i < max
      s[:i]
      s[:i]
      s[:i]
      s[:i]
      i += 1
    end
  }
}

puts "Benchmark FFI Struct.get(:int) using string name performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; max = iter / 4; while i < max
      s['i']
      s['i']
      s['i']
      s['i']
      i += 1
    end
  }
}

puts "Benchmark FFI Struct.put(:int) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; max = iter / 4; while i < max
      s[:i] = 0x12345678
      s[:i] = 0x12345678
      s[:i] = 0x12345678
      s[:i] = 0x12345678
      i += 1
    end
  }
}
puts "Benchmark FFI Struct.get(:pointer) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; max = iter / 4; while i < max
      s[:p]
      s[:p]
      s[:p]
      s[:p]
      i += 1
    end
  }
}
puts "Benchmark FFI Struct.put(:pointer) performance, #{iter}x"
10.times {
  p = FFI::MemoryPointer.new :int
  puts Benchmark.measure {
    i = 0; max = iter / 4; while i < max
      s[:p] = p
      s[:p] = p
      s[:p] = p
      s[:p] = p
      i += 1
    end
  }
}
puts "Benchmark FFI Struct.get(:string) performance, #{iter}x"
class StringStruct < FFI::Struct
  layout :s, :string
end
10.times {
  mp = FFI::MemoryPointer.new 1024
  mp.put_string(0, "Hello, World")
  s = StringStruct.new
  s.pointer.put_pointer(0, mp)
  puts Benchmark.measure {
    i = 0; while i < iter
      s[:s]
      i += 1
    end
  }
}
