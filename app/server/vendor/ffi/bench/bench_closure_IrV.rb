require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  callback :closureVrV, [ ], :void
  attach_function :ffi_bench, :testClosureIrV, [ :closureVrV, :int ], :void
  def self.rb_bench(i, &block); nil; end
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

puts "Benchmark [ ], :void closure block performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(1) { |i| } }
  }
}

puts "Benchmark [ ], :void pre-allocated function performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, [ :int ]) { |i| }
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 2) }
  }
}

puts "Benchmark [ ], :void pre-allocated callable performance, #{ITER}x calls"
10.times {
  fn = Proc.new { |i| }
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench(fn, 2) }
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(1) {} }
  }
}

