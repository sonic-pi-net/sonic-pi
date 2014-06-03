require 'benchmark'
require 'ffi'
require 'etc'

iter = 1000000

module Posix
  extend FFI::Library
  attach_function :getlogin, [], :string
end
if Posix.getlogin != Etc.getlogin
  raise ArgumentError, "FFI getlogin returned incorrect value"
end

puts "Benchmark FFI getlogin(2) performance, #{iter}x"

10.times {
  puts Benchmark.measure {
    iter.times { Posix.getlogin }
  }
}

puts "Benchmark Etc.getlogin performance, #{iter}x"
10.times {
  puts Benchmark.measure {
    iter.times { Etc.getlogin }
  }
}
