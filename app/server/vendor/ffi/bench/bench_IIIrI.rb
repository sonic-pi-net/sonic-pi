require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench, :bench_s32s32s32_v, [ :int, :int, :int ], :int
end


puts "Benchmark [ :int, :int, :int ], :void performance, #{ITER}x calls"

10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(0, 1, 2) }
  }
}
puts "Benchmark Invoker.call [ :int, :int, :int ], :void performance, #{ITER}x calls"

invoker = FFI.create_invoker(LIBTEST_PATH, 'bench_s32s32s32_v', [ :int, :int, :int ], :void)
10.times {
  puts Benchmark.measure {
    ITER.times { invoker.call(0, 1, 2) }
  }
}

