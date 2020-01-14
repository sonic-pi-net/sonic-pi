require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, 'bench_s32s32s32s32s32s32_v', [ :int, :int, :int, :int, :int, :int ], :void, :save_errno => false
  def self.rb_bench(i0, i1, i2, i3, i4, i5); nil; end
end

puts "Benchmark [ :int, :int, :int, :int, :int, :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.ffi_bench(1, 2, 3, 4, 5, 6)
      i += 1
    end
  }
}

puts "Benchmark ruby method(6 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.rb_bench(0, 1, 2, 3, 4, 5)
      i += 1
    end
  }
}

