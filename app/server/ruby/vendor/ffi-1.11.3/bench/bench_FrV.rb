require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_f32_v, [ :float ], :void
  def self.rb_bench(i0); nil; end
end


puts "Benchmark [ :float ], :void performance, #{ITER}x calls"
f = 1.0
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_f32_v(f)
      LibTest.bench_f32_v(f)
      LibTest.bench_f32_v(f)
      LibTest.bench_f32_v(f)
      i += 4
    end
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.rb_bench(f)
      LibTest.rb_bench(f)
      LibTest.rb_bench(f)
      LibTest.rb_bench(f)
      i += 4
    end
  }
}
