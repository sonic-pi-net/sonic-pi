require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :bench_s32_v, [ :int ], :void, :save_errno => false
  def self.rb_bench(i0); nil; end
end

puts "Benchmark [ :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      i += 4
    end
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      i += 4
    end
  }
}
