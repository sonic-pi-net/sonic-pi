#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
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
require 'fileutils'

require 'rbconfig'
ruby_api = RbConfig::CONFIG['ruby_version']
os = case RUBY_PLATFORM
     when /.*linux.*/
       if File.exist?('/etc/rpi-issue')
         :raspberry
       else
         :linux
       end
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
  [
    File.expand_path(File.dirname(__FILE__) + '/../vendor/rugged-1.7.2/ext/rugged'),
    "rugged"
  ],
  [
    File.expand_path(File.dirname(__FILE__) + '/../vendor/concurrent-ruby-1.2.2/ext/concurrent-ruby-ext'),
    "concurrent"
  ]
 ]


native_ext_dirs.each do |ext_dir|
  if ext_dir.is_a? Array
    ext_dir, tgt_dir = *ext_dir
  else
    tgt_dir = ""
  end
  puts "Compiling native extension in #{ext_dir}"
  Dir.chdir(ext_dir) do
    `"#{RbConfig.ruby}" extconf.rb`
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
