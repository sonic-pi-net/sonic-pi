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
require_relative "../sonicpi/lib/sonicpi/util"

include SonicPi::Util

tmp_dir = Dir.tmpdir

pids_store = tmp_dir + "/sonic-pi-pids"

unless File.exists? pids_store
  log_process_info "Creating pids store: #{pids_store}"
  Dir.mkdir(pids_store)
end

pid = ARGV[0]
pid_path = "#{pids_store}/#{pid}"

log_process_info "Started [#{pid}] - #{pid_path}"

FileUtils.touch pid_path
