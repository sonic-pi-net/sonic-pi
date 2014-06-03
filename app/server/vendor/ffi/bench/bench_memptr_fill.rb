require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

puts "Benchmark MemoryPointer#put_array_of_float performance, #{iter}x"

5.times {
  ptr = FFI::MemoryPointer.new(:float, 8, false)
  puts Benchmark.measure {
    iter.times { 
      ptr.put_array_of_float(0, [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ])
    }
  }
}


puts "Benchmark MemoryPointer.new(:float, 8, false)).put_array_of_float performance, #{iter}x"
5.times {
  puts Benchmark.measure {
    iter.times { 
      ptr = FFI::MemoryPointer.new(:float, 8, false)
      ptr.put_array_of_float(0, [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ])
    }
  }
}

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH

  attach_function :bench, :bench_p_v, [ :buffer_in ], :void
end

puts "Benchmark MemoryPointer alloc+fill+call performance, #{iter}x"
5.times {
  puts Benchmark.measure {
    iter.times {
      ptr = FFI::MemoryPointer.new(:float, 8, false)
      ptr.put_array_of_float(0, [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ])
      LibTest.bench(ptr)
    }
  }
}
