require File.expand_path(File.join(File.dirname(__FILE__), "bench_helper"))

module LibTest
  extend FFI::Library
  ffi_lib LIBTEST_PATH
  enum :foo, [ :a, :b, :c ]
  attach_function :ffi_bench, :bench_s32_v, [ :foo ], :void, :save_errno => true
  attach_function :ffi_bench_i, :bench_s32_v, [ :int ], :void, :save_errno => true
  def self.rb_bench(i0); nil; end
end

puts "Benchmark [ :int ], :void performance, #{ITER}x calls"
10.times {
  puts Benchmark.measure {
    ITER.times { LibTest.ffi_bench_i(1) }
  }
}
