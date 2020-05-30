require_relative 'bench_helper'

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  callback :closureVrV, [ ], :void
  attach_function :ffi_bench, :testClosureIrV, [ :closureVrV, :int ], :void
  def self.rb_bench(i, &block); nil; end
end

puts "Benchmark [ ], :void closure block performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(1) { |i| } }
  }
}

puts "Benchmark [ ], :void pre-allocated function performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, [ :int ]) { |i| }
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 2) }
  }
}

puts "Benchmark [ ], :void pre-allocated callable performance, #{ITER}x calls"
10.times {
  fn = Proc.new { |i| }
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 2) }
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(1) {} }
  }
}
