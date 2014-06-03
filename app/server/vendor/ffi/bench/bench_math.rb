require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module FFIMath
  extend FFI::Library
  ffi_lib 'm'
  attach_function :cos, [ :double ], :double
  attach_function :cosf, [ :float ], :float
end
if FFIMath.cos(0) != 1
  raise ArgumentError, "FFI.cos returned incorrect value"
end
puts "Benchmark FFI cos(0) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    ITER.times { FFIMath.cos(0) }
  }
}
puts "Benchmark FFI cosf(0) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    ITER.times { FFIMath.cosf(0) }
  }
}

puts "Benchmark Math.cos(0) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    ITER.times { Math.cos(0) }
  }
}
