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

## This script cleans up the environment after the server has finished
## It should be executed after starting the GUI/headless server

require_relative "../core.rb"
require_relative "../lib/sonicpi/util"

include SonicPi::Util

scripts = ["task-clear.rb"]

scripts.each do |s|
  full_path = File.absolute_path("#{server_bin_path}/#{s}")
  `'#{ruby_path}' '#{full_path}'`
  puts "Executed #{full_path}"
end
