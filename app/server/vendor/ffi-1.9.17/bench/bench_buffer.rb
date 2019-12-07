require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

module BufferBench
  extend FFI::Library
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_s32_v, [ :int ], :void
  begin
    attach_function :bench_buffer_in, :ptr_ret_int32_t, [ :buffer_in, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_buffer_in, :ptr_ret___int32_t, [ :buffer_in, :int ], :void
  end
  begin
    attach_function :bench_buffer_inout, :ptr_ret_int32_t, [ :buffer_inout, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_buffer_inout, :ptr_ret___int32_t, [ :buffer_inout, :int ], :void
  end
  begin
    attach_function :bench_buffer_out, :ptr_ret_int32_t, [ :buffer_out, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_buffer_out, :ptr_ret___int32_t, [ :buffer_out, :int ], :void
  end
end
class IntStruct < FFI::Struct
  layout :i, :int
end
puts "Benchmark FFI call(MemoryPointer.new(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::MemoryPointer.new(:int, 1, true), 0) }
  }
}
puts "Benchmark FFI call(MemoryPointer.new(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::MemoryPointer.new(4, 1, true), 0) }
  }
}
puts "Benchmark FFI call(MemoryPointer.new(IntStruct, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::MemoryPointer.new(IntStruct, 1, true), 0) }
  }
}
puts "Benchmark FFI call(0.chr * 4) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(0.chr * 4, 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_inout(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::Buffer.alloc_inout(:int, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_inout(IntStruct, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::Buffer.alloc_inout(IntStruct, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_inout(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_inout(FFI::Buffer.alloc_inout(4, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_in(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_in(FFI::Buffer.alloc_in(:int, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_in(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_in(FFI::Buffer.alloc_in(4, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_in(IntStruct, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_in(FFI::Buffer.alloc_in(IntStruct, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_out(:int, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_out(FFI::Buffer.alloc_out(:int, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_out(IntStruct, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_out(FFI::Buffer.alloc_out(IntStruct, 1, true), 0) }
  }
}
puts "Benchmark FFI call(Buffer.alloc_out(4, 1, true)) performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { BufferBench.bench_buffer_out(FFI::Buffer.alloc_out(4, 1, true), 0) }
  }
}

