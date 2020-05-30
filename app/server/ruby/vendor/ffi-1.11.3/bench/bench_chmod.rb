require_relative 'bench_helper'

iter = ITER
file = "README"

module Posix
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  def self.chmod(mode, path)
    if self._chmod(path, mode) != 0
    end
  end
  if FFI::Platform.windows?
    attach_function :_chmod, :_chmod, [ :string, :int ], :int
  else
    attach_function :_chmod, :chmod, [ :string, :int ], :int
  end
end


puts "Benchmark FFI chmod performance, #{iter}x changing mode"
10.times {
  puts Benchmark.measure {
    iter.times { Posix.chmod(0622, __FILE__) }
  }
}

puts "Benchmark Ruby File.chmod performance, #{iter}x changing mode"
10.times {
  puts Benchmark.measure {
    iter.times { File.chmod(0622, __FILE__) }
  }
}
