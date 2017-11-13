require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module Posix
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  attach_function :gettimeofday, [ :buffer_out, :pointer ], :int
end
class Timeval < FFI::Struct
  layout :tv_sec, :ulong, :tv_nsec, :ulong
end

iter = ITER
puts "Benchmark FFI gettimeofday(2) (nil, nil) performance, #{iter}x"

10.times {
  puts Benchmark.measure {
    iter.times { Posix.gettimeofday(nil, nil) }
  }
}
puts "Benchmark FFI gettimeofday(2) (Timeval.alloc_out, nil) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { Posix.gettimeofday(Timeval.alloc_out, nil) }
  }
}
puts "Benchmark FFI gettimeofday(2) (Timeval.new(FFI::MemoryPointer.new), nil) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { Posix.gettimeofday(Timeval.new(FFI::MemoryPointer.new(Timeval)), nil) }
  }
}
puts "Benchmark FFI gettimeofday(2) (Timeval.new(FFI::Buffer.new), nil) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { Posix.gettimeofday(Timeval.new(FFI::Buffer.new(Timeval)), nil) }
  }
}
puts "Benchmark FFI gettimeofday(2) (pre allocated pointer, nil) performance, #{iter}x"
10.times {
  t = Timeval.new FFI::MemoryPointer.new(Timeval)
  puts Benchmark.measure {
    iter.times { Posix.gettimeofday(t, nil) }
  }
}
puts "Benchmark Time.now performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { Time.now }
  }
}
