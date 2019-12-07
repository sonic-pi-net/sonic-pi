require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

puts "Benchmark Buffer.new(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::Buffer.new(:int, 1, true)
      i += 1
    end
  }
}

puts "Benchmark Buffer.alloc_out(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::Buffer.new_out(:int, 1, true)
      i += 1
    end
  }
}

puts "Benchmark Buffer.new(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::Buffer.new(4, 1, true)
      i += 1
    end
  }
}

puts "Benchmark Buffer.new(256, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::Buffer.new(256, 1, true)
      i += 1
    end
  }
}

