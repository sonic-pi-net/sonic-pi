require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :ffi_bench, :bench_s32s32s32_v, [ :int, :int, :int ], :void, :save_errno => false
  def self.rb_bench(i0, i1, i2); nil; end
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
    extern "void bench_s32s32s32_v(int, int, int)"
  end
end


puts "Benchmark [ :int, :int, :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.ffi_bench(0, 1, 2)
      i += 1
    end
  }
}

puts "Benchmark ruby method(3 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.rb_bench(0, 1, 2)
      i += 1
    end
  }
}
unless RUBY_PLATFORM == "java" && JRUBY_VERSION < "1.3.0"
puts "Benchmark DL void bench(int, int, int) performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    i = 0; while i < ITER
      LibTest.bench_s32s32s32_v(0, 1, 2)
      i += 1
    end
  }
}
end

