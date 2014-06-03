#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#
require 'rubygems'
require 'rbconfig'

if RUBY_PLATFORM =~/java/
  libdir = File.expand_path(File.join(File.dirname(__FILE__), "..", "..", "lib"))
  $:.reject! { |p| p == libdir }
else
  $:.unshift File.join(File.dirname(__FILE__), "..", "..", "lib"),
    File.join(File.dirname(__FILE__), "..", "..", "build", "#{RbConfig::CONFIG['host_cpu''arch']}", "ffi_c", RUBY_VERSION)
end
# puts "loadpath=#{$:.join(':')}"
require "ffi"

module TestLibrary
  PATH = "build/libtest.#{FFI::Platform::LIBSUFFIX}"
  def self.force_gc
    if RUBY_PLATFORM =~ /java/
      java.lang.System.gc
    elsif defined?(RUBY_ENGINE) && RUBY_ENGINE == 'rbx'
      GC.run(true)
    else
      GC.start
    end
  end
end
module LibTest
  extend FFI::Library
  ffi_lib TestLibrary::PATH
end
