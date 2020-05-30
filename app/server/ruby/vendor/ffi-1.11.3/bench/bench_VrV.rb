require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :returnVoid, [ ], :void
  attach_function :ffi_bench_noerrno, :returnVoid, [ ], :void, :save_errno => false
  def self.rb_bench; nil; end
end

puts "Benchmark [ ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0
    while i < ITER
      LibTest.ffi_bench
      i += 1
    end
  }
}

puts "Benchmark [ ], :void no-errno performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0
    while i < ITER
      LibTest.ffi_bench_noerrno
      i += 1
    end
  }
}

puts "Benchmark ruby method(no arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0
    while i < ITER
      LibTest.rb_bench
      i += 1
    end
  }
}

