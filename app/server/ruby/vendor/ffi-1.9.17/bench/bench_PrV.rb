require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH

  attach_function :bench, :bench_P_v, [ :buffer_in ], :void, :save_errno => false
end


puts "Benchmark [ :buffer_in ], :void performance (pre allocated pointer), #{ITER}x calls"
ptr = FFI::MemoryPointer.new :int
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      i += 4
    end
  }
}

puts "Benchmark [ :buffer_in ], :void performance (nil param), #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(nil)
      LibTest.bench(nil)
      LibTest.bench(nil)
      LibTest.bench(nil)
      i += 4
    end
  }
}

puts "Benchmark [ :buffer_in ], :void performance (pre allocated Buffer param), #{ITER}x calls"
ptr = FFI::Buffer.new :int
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      i += 4
    end
  }
}
puts "Benchmark [ :buffer_in ], :void performance (const String param), #{ITER}x calls"
ptr = 'test'
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      LibTest.bench(ptr)
      i += 4
    end
  }
}
puts "Benchmark [ :buffer_in ], :void performance (loop-allocated Buffer param), #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(FFI::Buffer.new(4))
      LibTest.bench(FFI::Buffer.new(4))
      LibTest.bench(FFI::Buffer.new(4))
      LibTest.bench(FFI::Buffer.new(4))
      i += 4
    end
  }
}
puts "Benchmark [ :buffer_in ], :void performance (loop-allocated String param), #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(String.new)
      LibTest.bench(String.new)
      LibTest.bench(String.new)
      LibTest.bench(String.new)
      i += 4
    end
  }
}
puts "Benchmark [ :buffer_in ], :void performance (loop-allocated MemoryPointer param), #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench(FFI::MemoryPointer.new(4))
      LibTest.bench(FFI::MemoryPointer.new(4))
      LibTest.bench(FFI::MemoryPointer.new(4))
      LibTest.bench(FFI::MemoryPointer.new(4))
      i += 4
    end
  }
}


