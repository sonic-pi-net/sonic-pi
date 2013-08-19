#!/usr/bin/env ruby


group_name = File.open("/tmp/sonic-pi/group-name").readlines.first.strip

["one", "two", "three", "four", "five", "six", "seven", "eight"].each do |n|
  dir_path = "~/.sonic-pi/workspaces/#{group_name}/#{n}"
  `mkdir -p #{dir_path}`
  `touch #{dir_path}/1.spi`
end
