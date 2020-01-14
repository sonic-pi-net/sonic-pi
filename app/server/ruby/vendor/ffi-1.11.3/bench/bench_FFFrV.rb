require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_f32f32f32_v, [ :float, :float, :float ], :void
  def self.rb_bench(a0, a1, a2); nil; end
end


puts "Benchmark [ :float, :float, :float ], :void performance, #{ITER}x calls"
f = 1.0
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench_f32f32f32_v(f, f, f) }
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(f, f, f) }
  }
}
