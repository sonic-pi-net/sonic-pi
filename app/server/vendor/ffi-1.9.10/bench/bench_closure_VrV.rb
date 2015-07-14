require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  callback :closureVrV, [ ], :void
  
  attach_function :ffi_bench, :testClosureVrV, [ :closureVrV ], :void
  @blocking = true
  attach_function :threaded_bench, :testThreadedClosureVrV, [ :closureVrV, :int ], :void
  
  def self.rb_bench(&block)
    yield
  end
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
    ITER.times { LibTest.ffi_bench { } }
  }
}

puts "Benchmark [ ], :void pre-allocated function pointer performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, []) {}
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench fn }
  }
}

puts "Benchmark [ ], :void pre-allocated closure performance, #{ITER}x calls"
10.times {
  proc = lambda {}
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench proc }
  }
}

puts "Benchmark [ ], :void non-ruby thread closure performance, #{ITER}x calls"
10.times {
  fn = FFI::Function.new(:void, []) {}
  puts Benchmark.measure {
    LibTest.threaded_bench(fn, ITER)
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

puts "Benchmark ruby method(no arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench {} }
  }
}

