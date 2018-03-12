require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'

module StructBench
  extend FFI::Library
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_s32_v, [ :int ], :void
  begin
    attach_function :bench_struct_in, :ptr_ret_int32_t, [ :buffer_in, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_struct_in, :ptr_ret___int32_t, [ :buffer_in, :int ], :void
  end
  begin
    attach_function :bench_struct_out, :ptr_ret_int32_t, [ :buffer_out, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_struct_out, :ptr_ret___int32_t, [ :buffer_out, :int ], :void
  end
  begin
    attach_function :bench_struct_inout, :ptr_ret_int32_t, [ :buffer_inout, :int ], :void
  rescue FFI::NotFoundError
    # NetBSD uses #define instead of typedef for these
    attach_function :bench_struct_inout, :ptr_ret___int32_t, [ :buffer_inout, :int ], :void
  end
end
class TestStruct < FFI::Struct
  layout :i, :int, :p, :pointer
end

puts "Benchmark FFI call(Struct.alloc_in) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      StructBench.bench_struct_in(TestStruct.alloc_in, 0)
      i += 1
    end
  }
}

puts "Benchmark FFI call(Struct.alloc_out) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      StructBench.bench_struct_out(TestStruct.alloc_out, 0)
      i += 1
    end
  }
}

puts "Benchmark FFI call(Struct.alloc_inout) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      StructBench.bench_struct_inout(TestStruct.alloc_inout, 0)
      i += 1
    end
  }
}
