#!/usr/bin/ruby -wU

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

require 'fileutils'

# Simple script to nuke all non lib dirs in the vendor tree. Useful for
# pruning an app pre release. Be very Careful as it will nuke everything
# recursively in a dir. Has a basic safety check (looks for
# app/server/vendor in the path) and by default doesn't do anything
# unless you change rehearse to false.  Try not to nuke the wrong
# things!

# Call with:
# ./prune.rb path/to/vendor/dir

dir = ARGV[0]

rehearse = true


subdirs = Dir["#{dir}/*/*"].select{|d| File.directory?(d) && (File.basename(d) != "lib")}

puts ""


raise "Aborting prune. Doesn't look like you're using an app/server/ruby/vendor dir" unless subdirs.first.match(/app\/server\/ruby\/vendor/)

if rehearse
  puts "Would remove: "
  puts subdirs
  puts "Aborting prune operation. Turn rehearse off to remove files."
else
  subdirs.each do |d|
    puts "rm -rf #{d}"
    FileUtils.rm_rf d
  end
end
