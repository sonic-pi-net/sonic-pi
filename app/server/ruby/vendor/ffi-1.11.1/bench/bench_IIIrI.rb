require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench, :bench_s32s32s32_v, [ :int, :int, :int ], :int
end


puts "Benchmark [ :int, :int, :int ], :int performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench(0, 1, 2) }
  }
}
