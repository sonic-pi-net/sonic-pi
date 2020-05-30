require 'benchmark'
require 'rbconfig'

lib = File.expand_path('../../lib', __FILE__)

cext = "#{lib}/ffi_c.#{RbConfig::CONFIG['DLEXT']}"
unless File.exist?(cext)
  abort "#{cext} is not compiled. Compile it with 'rake compile' first."
end

$LOAD_PATH.unshift(lib)
require 'ffi'

require_relative '../spec/ffi/fixtures/compile'

ITER = ENV['ITER'] ? ENV['ITER'].to_i : 100_000

LIBTEST_PATH = TestLibrary::PATH
