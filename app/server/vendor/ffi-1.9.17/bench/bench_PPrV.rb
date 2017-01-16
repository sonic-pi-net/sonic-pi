require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH

  attach_function :bench, :bench_PP_v, [ :buffer_in, :buffer_in ], :void
end


puts "Benchmark [ :buffer_in, :pointer ], :void performance, #{ITER}x calls"
ptr = FFI::MemoryPointer.new :int
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(ptr, ptr) }
  }
}
puts "Benchmark [ :buffer_in, :buffer_in ], :void with Struct parameters performance #{ITER}x calls"

class TestStruct < FFI::Struct
  layout :i, :int
end

s = TestStruct.new(FFI::MemoryPointer.new(TestStruct));
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(s, s) }
  }
}

puts "Benchmark [ :buffer_in, :buffer_in ], :void with nil parameters performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(nil, nil) }
  }
}


puts "Benchmark [ :buffer_in, :buffer_in ], :void with Buffer parameters performance, #{ITER}x calls"
ptr = FFI::Buffer.new(:int)
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(ptr, ptr) }
  }
}

puts "Benchmark [ :buffer_in, :buffer_in ], :void with one nil, one Buffer parameter performance, #{ITER}x calls"
ptr = FFI::Buffer.new(:int)
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(nil, ptr) }
  }
}

puts "Benchmark [ :buffer_in, :buffer_in ], :void with loop-allocated Buffer parameters performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(FFI::Buffer.new(:int), FFI::Buffer.new(:int)) }
  }
}

puts "Benchmark [ :buffer_in, :buffer_in ], :void with loop-allocated MemoryPointer parameters performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(FFI::MemoryPointer.new(:int), FFI::MemoryPointer.new(:int)) }
  }
}


