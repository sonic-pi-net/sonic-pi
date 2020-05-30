require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :returnInt, [ ], :int, :save_errno => false
  def self.rb_bench; 0xdeadbeef; end
end

puts "Benchmark [ ], :int performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; max = ITER / 4
    while i < max
      LibTest.ffi_bench
      LibTest.ffi_bench
      LibTest.ffi_bench
      LibTest.ffi_bench
      i += 1
    end
  }
}

puts "Benchmark ruby method(), nil performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; max = ITER / 4
    while i < max
      LibTest.rb_bench
      LibTest.rb_bench
      LibTest.rb_bench
      LibTest.rb_bench
      i += 1
    end
  }
}
