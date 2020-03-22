#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# This file will build all native dependencies required by the vendored
# libraries. Ensure you execute this file with the specific version of
# Ruby you intend to package with Sonic Pi.

# for extensions that need rake-compiler
Dir["#{File.expand_path("../../vendor", __FILE__)}/rake-compiler-1.1.0/lib"].each do |vendor_lib|
  $LOAD_PATH.unshift vendor_lib
end
require 'rake'
require (File.expand_path(File.dirname(__FILE__) + '/../vendor/rake-compiler-1.1.0/lib/rake/extensiontask'))

require 'fileutils'

require 'rbconfig'
ruby_api = RbConfig::CONFIG['ruby_version']
os = case RUBY_PLATFORM
     when /.*arm.*-linux.*/
       :raspberry
     when /.*linux.*/
       :linux
     when /.*darwin.*/
       :osx
     when /.*mingw.*/
       :windows
     else
       RUBY_PLATFORM
     end

native_dir = File.dirname(__FILE__) + "/../rb-native/#{ruby_api}"
puts "Clearing #{native_dir}"
FileUtils.rm_rf native_dir
puts "Creating #{native_dir}"
FileUtils.mkdir_p native_dir

# Rugged is used for storing the user's ruby music scripts in Git
# FFI is used for MIDI lib support
native_ext_dirs = [
  File.expand_path(File.dirname(__FILE__) + '/../vendor/rugged-0.28.4.1/ext/rugged'),
  File.expand_path(File.dirname(__FILE__) + '/../vendor/ffi-1.11.3/ext/ffi_c/'),
  File.expand_path(File.dirname(__FILE__) + '/../vendor/atomic/ext'),
  File.expand_path(File.dirname(__FILE__) + '/../vendor/ruby-prof-0.15.8/ext/ruby_prof/'),
  File.expand_path(File.dirname(__FILE__) + '/../vendor/interception/ext/'),
]

rake_compiler_dirs = [
  File.expand_path(File.dirname(__FILE__) + '/../vendor/fast_osc-1.2.2/')
]

if os == :osx
  native_ext_dirs += [
    File.expand_path(File.dirname(__FILE__) + '/../vendor/narray-0.6.1.1/'),
    File.expand_path(File.dirname(__FILE__) + '/../vendor/ruby-coreaudio-0.0.12-patched/ext/coreaudio/')
  ]
end

rake_compiler_dirs.each do |rake_compiler_dir|
  puts "Compiling native extension in #{rake_compiler_dir}"

  spec = Gem::Specification.load(File.expand_path(rake_compiler_dir + 'fast_osc.gemspec'))
  Rake::ExtensionTask.new('fast_osc') do |ext|
    # ext.platform or ext.cross_config_options
    # might work to enable universal builds on darwin for older processors
    ext.lib_dir = "lib/fast_osc"
  end

  Dir.chdir(rake_compiler_dir) do
    app = Rake.application
    app.init
    # this loads the Rakefile and other imports
    app.load_rakefile

    if os == :windows
      begin
        app.rake_require('devkit')
      rescue LoadError
        $stderr.puts "WARNING: couldn't find devkit"
        $stderr.puts "If you're using RubyInstaller check that devkit is installed"
      end
    end

    begin
      app['clean'].invoke
      app['compile'].invoke
    rescue
      $stderr.puts "WARNING: couldn't compile FastOsc extension"
      $stderr.puts "Ruby fallback will be used instead"
    end
  end
end


native_ext_dirs.each do |ext_dir|
  if ext_dir.is_a? Array
    ext_dir, tgt_dir = *ext_dir
  else
    tgt_dir = ""
  end
  puts "Compiling native extension in #{ext_dir}"
  Dir.chdir(ext_dir) do
    `#{RbConfig.ruby} extconf.rb`
    `make clean`
    `make`
  end

  libs = []
  case os
  when :raspberry
    libs = Dir[ext_dir + '/*.{so}']
  when :linux
    libs = Dir[ext_dir + '/*.{so}']
  when :osx
    libs = Dir[ext_dir + '/*.{bundle}']
  when :windows
    libs = Dir[ext_dir + '/*.{so}']
  end

  libs.each do |f|
    tgt = "#{native_dir}/#{tgt_dir}"
    FileUtils.mkdir_p tgt
    puts "Copying #{f} to #{tgt}"
    FileUtils.cp f, tgt
  end
end

