require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

require 'benchmark'
require 'ffi'
iter = ITER

class Ptr < FFI::AutoPointer
  def self.release(ptr)
    LibC.free(ptr);
  end
end

module LibC
  extend FFI::Library
  ffi_lib FFI::Library::LIBC
  attach_function :malloc, [ :long ], Ptr, :ignore_error => true
  attach_function :malloc2, :malloc, [ :long ], :pointer, :ignore_error => true
  attach_function :free, [ :pointer ], :void, :ignore_error => true
  def self.finalizer(ptr) 
    proc { LibC.free(ptr) }
  end
end


puts "Benchmark AutoPointer.new performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { LibC.malloc(4) }
  }
}

puts "Benchmark ObjectSpace finalizer performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { 
      ptr = LibC.malloc2(4)
      ptr2 = FFI::Pointer.new(ptr)
      ObjectSpace.define_finalizer(ptr2, LibC.finalizer(ptr))
    }
  }
}

