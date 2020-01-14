require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  enum :foo, [ :a, :b, :c ]
  attach_function :ffi_bench, :bench_s32_v, [ :foo ], :void, :save_errno => false
  attach_function :ffi_bench_i, :bench_s32_v, [ :int ], :void, :save_errno => false
  def self.rb_bench(i0); nil; end
end

puts "Benchmark [ enum ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(:a) }
  }
}

puts "Benchmark [ enum ], :void with int arg performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(1) }
  }
}

puts "Benchmark [ :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench_i(1) }
  }
}

puts "Benchmark [ :int ], :void with enum arg performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench_i(:a) }
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(:a) }
  }
}
