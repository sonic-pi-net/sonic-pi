require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER
class Foo < FFI::Buffer
  def initialize(a1, a2, a3, a4, a5)
    puts "Foo#initialize(#{a1}, #{a2}. #{a3}, #{a4}, #{a5})"
  end
 
end

Foo.new(1, 2, 3, 4, 5)

#FFI::Buffer.fubar(:int, 1, true)

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

