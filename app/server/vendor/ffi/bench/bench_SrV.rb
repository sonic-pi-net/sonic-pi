require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

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
puts "Benchmark Invoker.call [ :string  ], :void performance, #{ITER}x calls"

invoker = FFI.create_invoker(LIBTEST_PATH, 'bench_S_v', [ :string ], :void)
unless invoker.respond_to?("call1")
  class FFI::Invoker
    alias :call1 :call
  end
end
10.times {
  puts Benchmark.measure {
    ITER.times { invoker.call1(s) }
  }
}
