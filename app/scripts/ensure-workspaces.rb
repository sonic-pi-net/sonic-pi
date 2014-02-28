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

group_name="default"

["one", "two", "three", "four", "five", "six", "seven", "eight"].each do |n|
  dir_path = "~/.sonic-pi/workspaces/#{group_name}/#{n}"
  `mkdir -p #{dir_path}`
  `touch #{dir_path}/1.spi`
end
