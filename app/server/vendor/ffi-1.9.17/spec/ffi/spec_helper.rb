#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require 'rbconfig'
require 'fileutils'
require 'ffi'

RSpec.configure do |c|
  c.filter_run_excluding :broken => true
end

CPU = case RbConfig::CONFIG['host_cpu'].downcase
  when /i[3456]86/
    # Darwin always reports i686, even when running in 64bit mode
    if RbConfig::CONFIG['host_os'] =~ /darwin/ && 0xfee1deadbeef.is_a?(Fixnum)
      "x86_64"
    else
      "i386"
    end

  when /amd64|x86_64/
    "x86_64"

  when /ppc64|powerpc64/
    "powerpc64"

  when /ppc|powerpc/
    "powerpc"

  when /^arm/
    "arm"

  else
    RbConfig::CONFIG['host_cpu']
  end

OS = case RbConfig::CONFIG['host_os'].downcase
  when /linux/
    "linux"
  when /darwin/
    "darwin"
  when /freebsd/
    "freebsd"
  when /openbsd/
    "openbsd"
  when /sunos|solaris/
    "solaris"
  when /mswin|mingw/
    "win32"
  else
    RbConfig::CONFIG['host_os'].downcase
  end

def compile_library(path, lib)

  dir = File.expand_path(path, File.dirname(__FILE__))
  lib = "#{dir}/#{lib}"
  if !File.exist?(lib)
    output = nil
    FileUtils.cd(dir) do
      output = system(*%{#{system('which gmake >/dev/null') && 'gmake' || 'make'} CPU=#{CPU} OS=#{OS} }.tap{|x| puts x.inspect})
    end

    if $?.exitstatus != 0
      puts "ERROR:\n#{output}"
      raise "Unable to compile \"#{lib}\""
    end
  end

  lib
end

require "ffi"

module TestLibrary
  PATH = compile_library("fixtures", "libtest.#{FFI::Platform::LIBSUFFIX}")

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
