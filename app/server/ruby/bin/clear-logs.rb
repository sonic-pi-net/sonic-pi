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
require 'tmpdir'
require 'fileutils'
require 'time'

require_relative "../core.rb"
require_relative "../lib/sonicpi/util"

include SonicPi::Util

# Windows doesn't allow certain chars in file paths
# which are present in the default Time.now string format.
# therefore remove them.
sanitised_time_str = Time.now.to_s.gsub!(/[<>:|?*]/, '_')
history_dir = "#{log_path}/history/#{sanitised_time_str}"
FileUtils.mkdir_p(history_dir)

Dir["#{log_path}/*.log"].each do |p|
  # Copy log to history directory
  FileUtils.cp(p, "#{history_dir}/")
  # Clear out all logs (don't remove the files, just empty them)
  File.open(p, 'w') {|file| file.truncate(0) }
end

# clean up old history logs
# only store the last 10 sessions
num_sessions_to_store = 10

history_dirs = Dir.glob("#{log_path}/history/*")

timestamps = history_dirs.map do |d|
  begin
    Time.parse File.basename(d)
  rescue
    nil
  end
end

timestamps.compact!
num_timestamps = timestamps.size
num_to_drop = num_timestamps - num_sessions_to_store

if num_to_drop.positive?
  timestamps.sort.take(num_to_drop).each do |ts|
    dir = ts.to_s
    FileUtils.rm_rf("#{log_path}/history/#{dir}")
  end
end
