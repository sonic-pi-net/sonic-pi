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
require 'tmpdir'
require 'fileutils'

require_relative "../core.rb"
require_relative "../lib/sonicpi/util"

include SonicPi::Util

# Clear out all logs (don't remove the files, just empty them)
Dir["#{log_path}/*"].each do |p|
  File.open(p, 'w') {|file| file.truncate(0) }
end
