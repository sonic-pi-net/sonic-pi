require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :returnInt, [ ], :int, :save_errno => false
  def self.rb_bench; 0xdeadbeef; end
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

puts "Benchmark [ ], :int performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; max = ITER / 4
    while i < max
      LibTest.ffi_bench
      LibTest.ffi_bench
      LibTest.ffi_bench
      LibTest.ffi_bench
      i += 1
    end
  }
}

puts "Benchmark ruby method(), nil performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; max = ITER / 4
    while i < max
      LibTest.rb_bench
      LibTest.rb_bench
      LibTest.rb_bench
      LibTest.rb_bench
      i += 1
    end
  }
}

unless RUBY_PLATFORM == "java" && JRUBY_VERSION < "1.3.0"
puts "Benchmark DL void bench() performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; max = ITER / 4
    while i < max
      LibTest.returnInt
      LibTest.returnInt
      LibTest.returnInt
      LibTest.returnInt
      i += 1
    end
  }
}
end
