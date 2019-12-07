require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

module LibC
  extend FFI::Library
  ffi_lib 'c'
  attach_function :calloc, [ :size_t, :size_t ], :pointer, :save_errno => false
  attach_function :free, [ :pointer ], :void, :save_errno => false
end

puts "Benchmark calloc(1, 4) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      ptr = LibC.calloc(1, 4)
      LibC.free(ptr)
      i += 1
    end
  }
}
puts "Benchmark MemoryPointer.new(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::MemoryPointer.new(:int)
      i += 1
    end
  }
}
puts "Benchmark MemoryPointer.new(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::MemoryPointer.new(4, 1, true)
      i += 1
    end
  }
}
[ 8, 16, 32, 64, 128, 256 ].each do |size|
puts "Benchmark MemoryPointer.new(#{size}, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      FFI::MemoryPointer.new(size, 1, true)
      i += 1
    end
  }
}
end

if defined?(RUBY_ENGINE) && RUBY_ENGINE == "jruby"
  require 'java'
  puts "calling java gc"
  10.times { 
    java.lang.System.gc 
    sleep 1
  }
end

