require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_S_v, [ :string ], :void
end

puts "Benchmark [ :string ], :void performance, #{ITER}x calls"

s = 'a' * 1000
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench_S_v(s) }
  }
}
