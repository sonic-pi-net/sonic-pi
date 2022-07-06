#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2021 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'fileutils'
require_relative '../paths'


# Copy contents of current logs to a rotating
# backup directory and clear ready for a new
# run.
#
# TODO: Handle the case where the log path isn't writable.


# ensure this list matches set of expected log files should more services/aspects be similarly logged.
expected_logs = [
  "daemon.log",
  "debug.log",
  "gui.log",
  "scsynth.log",
  "spider.log",
  "tau.log"]

# Windows doesn't allow certain chars in file paths
# which are present in the default Time.now string format.
# therefore remove them.
sanitised_time_str = Time.now.to_s.gsub!(/[^0-9a-zA-Z-]/, '_')
history_dir = File.absolute_path("#{SonicPi::Paths.log_history_path}/#{sanitised_time_str}")

FileUtils.mkdir_p(history_dir)

Dir["#{SonicPi::Paths.log_path}/*.log"].each do |p|
  FileUtils.cp(p, "#{history_dir}/")
  # Copy log to history directory
  if expected_logs.include?(File.basename(p))
    # (don't remove the expected log files, just empty them)
    File.open(p, 'w') {|file| file.truncate(0) }
  else
    begin
      FileUtils.rm p
    rescue
    end
  end
end

# clean up old history logs
# only store the last 10 sessions
num_sessions_to_store = 10

history_dirs = Dir.glob("#{SonicPi::Paths.log_history_path}/*")

begin
  history_dirs = history_dirs.sort {|d| File.birthtime(d) }
rescue
  nil
end

num_history_dirs = history_dirs.size
num_to_drop = num_history_dirs - num_sessions_to_store

if num_to_drop.positive?
  history_dirs.take(num_to_drop).each do |dir_path|
    FileUtils.rm_rf(dir_path)
  end
end

puts ':ok'
