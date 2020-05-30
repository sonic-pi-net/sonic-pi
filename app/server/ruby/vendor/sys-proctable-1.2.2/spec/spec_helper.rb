RSpec.configure do |config|
  config.filter_run_excluding :skip_jruby if RUBY_PLATFORM == 'java'
end

if RUBY_PLATFORM == 'java'
  require 'ffi'
  module Exec
    extend FFI::Library
    ffi_lib FFI::Library::LIBC
    attach_function :fork, [], :int
  end

  def fork
    pid = Exec.fork
    if pid == 0
      yield if block_given?
      return nil
    else
      return pid
    end
  end
end
