require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  callback :closureIIIrV, [ :int, :int, :int ], :void
  attach_function :ffi_bench, :testClosureIIIrV, [ :closureIIIrV, :int, :int, :int ], :void
  def self.rb_bench(a, b, c, &block); yield(a, b, c); end
end
unless RUBY_PLATFORM == "java" && JRUBY_VERSION < "1.3.0"
  require 'dl'
  require 'dl/import'
  module LibTest
    if RUBY_VERSION >= "1.9.0"
      extend DL::Importer
    else
      extend DL::Importable
    end
    dlload LIBTEST_PATH
    extern "int returnInt()"
  end
end

class Foo
  def call(a, b, c); nil; end
end

puts "Benchmark [ ], :void closure block, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(1, 2, 3) { } }
  }
}

puts "Benchmark [ ], :void closure callable, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(Foo.new, 1, 2, 3) }
  }
}


puts "Benchmark [ ], :void pre-allocated function with block, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, [ :int, :int, :int ]) {}
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 1, 2, 3) }
  }
}

puts "Benchmark [ ], :void pre-allocated function with callable, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, [ :int, :int, :int ], Foo.new)
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 1, 2, 3) }
  }
}

puts "Benchmark [ ], :void pre-allocated proc, #{ITER}x calls"
10.times {
  proc = lambda { |a,b,c| }
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(proc, 1, 2, 3) }
  }
}

puts "Benchmark [ ], :void pre-allocated callable, #{ITER}x calls"
10.times {
  proc = Foo.new
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(proc, 1, 2, 3) }
  }
}

#unless RUBY_PLATFORM == "java" && JRUBY_VERSION < "1.3.0"
#puts "Benchmark DL void bench() performance, #{ITER}x calls"
#10.times {
#  puts Benchmark.measure {
#    ITER.times { LibTest.returnInt }
#  }
#}
#end

puts "Benchmark ruby method(3 arg), #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(1, 2, 3) {} }
  }
}

