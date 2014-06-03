require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  attach_function :bench_f32f32f32_v, [ :float, :float, :float ], :void
  def self.rb_bench(a0, a1, a2); nil; end
end


puts "Benchmark [ :float, :float, :float ], :void performance, #{ITER}x calls"
f = 1.0
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.bench_f32f32f32_v(f, f, f) }
  }
}

puts "Benchmark ruby method(1 arg)  performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.rb_bench(f, f, f) }
  }
}

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
    extern "void bench_f32f32f32_v(float, float, float)"
  end
  puts "Benchmark DL void bench(float, float, float) performance, #{ITER}x calls"
  10.times {
  f1 = 1.0; f2 = 2.0; f3 = 3.0
    puts Benchmark.measure {
      ITER.times { LibTest.bench_f32f32f32_v(f1, f2, f3) }
    }
  }
end

