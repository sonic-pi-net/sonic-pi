require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  callback :closureVrV, [ ], :void

  attach_function :ffi_bench, :testClosureVrV, [ :closureVrV ], :void
  @blocking = true
  attach_function :threaded_bench, :testThreadedClosureVrV, [ :closureVrV, :int ], :void

  def self.rb_bench(&block)
    yield
  end
end

puts "Benchmark [ ], :void closure block performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench { } }
  }
}

puts "Benchmark [ ], :void pre-allocated function pointer performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, []) {}
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench fn }
  }
}

puts "Benchmark [ ], :void pre-allocated closure performance, #{ITER}x calls"
10.times {
  proc = lambda {}
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench proc }
  }
}

puts "Benchmark [ ], :void non-ruby thread closure performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, []) {}
  puts Benchmark.measure {
    LibTest.threaded_bench(fn, ITER)
  }
}

puts "Benchmark ruby method(no arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench {} }
  }
}
