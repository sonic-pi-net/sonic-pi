require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module Posix
  extend FFI::Library
  ffi_lib 'c'
  attach_function 'umask', [ :int ], :int
end
module NativeFile
  extend FFI::Library
  ffi_lib 'c'
  # Attaching the function to this module is about 10% faster than calling Posix.umask
  if FFI::Platform.windows?
    attach_function :_umask, '_umask', [ :int ], :int
  else
    attach_function :_umask, 'umask', [ :int ], :int
  end
  def self.umask(mask = nil)
    if mask
      _umask(mask)
    else
      old = _umask(0)
      _umask(old)
      old
    end
  end
end
puts "FFI umask=#{NativeFile.umask} File.umask=#{File.umask}"
puts "Benchmark File.umask(0777) performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    ITER.times { File.umask(0777) }
  }
}
puts "Benchmark FFI File.umask(0777) performance, #{ITER}x"

10.times {
  puts Benchmark.measure {
    ITER.times { NativeFile.umask(0777) }
  }
}
puts "Benchmark FFI Posix.umask(0777) performance, #{ITER}x"

10.times {
  puts Benchmark.measure {
    ITER.times { Posix.umask(0777) }
  }
}
puts "Benchmark File.umask() performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    ITER.times { File.umask }
  }
}
puts "Benchmark FFI File.umask() performance, #{ITER}x"

10.times {
  puts Benchmark.measure {
    ITER.times { NativeFile.umask }
  }
}
