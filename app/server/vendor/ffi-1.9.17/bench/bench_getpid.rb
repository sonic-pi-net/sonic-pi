require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

iter = ITER

module Posix
  extend FFI::Library
  ffi_lib 'c'
  if FFI::Platform.windows?
    attach_function :getpid, :_getpid, [], :uint, :save_errno => false
  else
    attach_function :getpid, [], :uint, :save_errno => false
  end
end


puts "pid=#{Process.pid} Foo.getpid=#{Posix.getpid}"
puts "Benchmark FFI getpid performance, #{iter}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      Posix.getpid
      i += 1
    end
  }
}
puts "Benchmark Process.pid performance, #{iter}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < iter
      Process.pid
      i += 1
    end
  }
}
