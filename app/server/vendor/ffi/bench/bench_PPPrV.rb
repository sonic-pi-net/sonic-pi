require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH

  attach_function :bench_ptr, :bench_PPP_v, [ :pointer, :pointer, :pointer ], :void, :save_errno => false
  attach_function :bench_buffer, :bench_PPP_v, [ :pointer, :pointer, :pointer ], :void, :save_errno => false
  attach_function :bench_struct, :bench_PPP_v, [ :pointer, :pointer, :pointer ], :void, :save_errno => false
  attach_function :bench_nil, :bench_PPP_v, [ :pointer, :pointer, :pointer ], :void, :save_errno => false
  attach_function :bench_conv, :bench_PPP_v, [ :pointer, :pointer, :pointer ], :void, :save_errno => false
end


puts "Benchmark [ :pointer, :pointer, :pointer ], :void performance, #{ITER}x calls"
ptr = FFI::MemoryPointer.new :int
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      i += 4
    end
  }
}
puts "Benchmark [ :pointer, :pointer, :pointer ], :void with Struct parameters performance #{ITER}x calls"

class TestStruct < FFI::Struct
  layout :i, :int
end

s = TestStruct.new(FFI::MemoryPointer.new(TestStruct));
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_struct(s, s, s)
      LibTest.bench_struct(s, s, s)
      LibTest.bench_struct(s, s, s)
      LibTest.bench_struct(s, s, s)
      i += 4
    end
  }
}

puts "Benchmark [ :pointer, :pointer, :pointer ], :void with Buffer parameters performance, #{ITER}x calls"
ptr = FFI::Buffer.new(:int)
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_buffer(ptr, ptr, ptr)
      LibTest.bench_buffer(ptr, ptr, ptr)
      LibTest.bench_buffer(ptr, ptr, ptr)
      LibTest.bench_buffer(ptr, ptr, ptr)
      i += 4
    end
  }
}

puts "Benchmark [ :pointer, :pointer, :pointer ], :void with nil parameters performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_nil(nil, nil, nil)
      LibTest.bench_nil(nil, nil, nil)
      LibTest.bench_nil(nil, nil, nil)
      LibTest.bench_nil(nil, nil, nil)
      i += 4
    end
  }
}



puts "Benchmark [ :pointer, :pointer, :pointer ], :void with string parameters performance, #{ITER}x calls"
ptr = 0.chr * 4
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      LibTest.bench_ptr(ptr, ptr, ptr)
      i += 4
    end
  }
}

class PointerType
  def initialize(ptr)
    @pointer = ptr
  end

  def to_ptr
    @pointer
  end
end

puts "Benchmark [ :pointer, :pointer, :pointer ], :void with to_ptr converting parameters performance, #{ITER}x calls"
ptr = PointerType.new(FFI::MemoryPointer.new(:int))
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_conv(ptr, ptr, ptr)
      LibTest.bench_conv(ptr, ptr, ptr)
      LibTest.bench_conv(ptr, ptr, ptr)
      LibTest.bench_conv(ptr, ptr, ptr)
      i += 4
    end
  }
}
