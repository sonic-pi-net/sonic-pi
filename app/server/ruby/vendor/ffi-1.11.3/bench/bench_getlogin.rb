require_relative 'bench_helper'
require 'etc'

iter = ITER

module Posix
  extend FFI::Library
  ffi_lib FFI::Library::LIBC
  attach_function :getlogin, [], :string
end
if Posix.getlogin != Etc.getlogin
  raise ArgumentError, "FFI getlogin returned incorrect value"
end

puts "Benchmark FFI getlogin(2) performance, #{ITER}x"

10.times {
  puts Benchmark.measure {
    iter.times { Posix.getlogin }
  }
}

puts "Benchmark Etc.getlogin performance, #{ITER}x"
10.times {
  puts Benchmark.measure {
    iter.times { Etc.getlogin }
  }
}
