#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'fileutils'

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

native_dir = File.dirname(__FILE__) + '/../rb-native/' + os.to_s + '/' + "#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}"
puts "creating #{native_dir}"
FileUtils.mkdir_p native_dir

native_ext_dirs = [
  File.expand_path(File.dirname(__FILE__) + '/../vendor/rugged/ext/rugged'),
  File.expand_path(File.dirname(__FILE__) + '/../vendor/ffi/ext/ffi_c')]


native_ext_dirs.each do |ext_dir|
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
    libs = Dir[ext_dir + '/*.{dll}']
  end

  libs.each do |f|
    puts "Copying #{f}  to #{native_dir}"
    FileUtils.cp f, native_dir
  end

end
