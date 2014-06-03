require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :bench_s32_v, [ :int ], :void, :save_errno => false
  def self.rb_bench(i0); nil; end
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
    extern "void bench_s32_v(int)"
  end
end

puts "Benchmark [ :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      LibTest.ffi_bench(0)
      i += 4
    end
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      LibTest.rb_bench(0)
      i += 4
    end
  }
}

unless RUBY_PLATFORM == "java" && JRUBY_VERSION < "1.3.0"
puts "Benchmark DL void bench(int) performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_s32_v(0)
      i += 1
    end
  }
}
end


