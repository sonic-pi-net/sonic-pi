require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER || 1000_000

class TestStruct < FFI::Struct
  layout :i, :int, :p, :pointer
end

puts "Benchmark FFI Struct class size performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { TestStruct.size }
  }
}

s = TestStruct.new(FFI::MemoryPointer.new(TestStruct))
puts "Benchmark FFI Struct instance size performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { s.size }
  }
}

puts "Benchmark FFI Struct layout size performance, #{iter}x"
layout = TestStruct.layout
10.times {
  puts Benchmark.measure {
    iter.times { layout.size }
  }
}

