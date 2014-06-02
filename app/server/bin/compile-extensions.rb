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

native_dir = File.dirname(__FILE__) + '/../rb-native/' + os + '/' + RUBY_VERSION
puts "creating #{native_dir}"
FileUtils.mkdir_p native_dir
rugged_dir = File.expand_path(File.dirname(__FILE__) + '/../vendor/rugged/ext/rugged')

Dir.chdir(rugged_dir) do
  `ruby #{rugged_dir}/extconf.rb`
  `make clean`
  `make`
end

Dir[rugged_dir + '/*.{so,bundle}'].each do |f|
  FileUtils.cp f, native_dir
end
