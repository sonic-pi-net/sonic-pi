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
require_relative "../core"
require_relative "../lib/sonicpi/util"
require 'sys-proctable'

include SonicPi::Util

tmp_dir = Dir.tmpdir

pids_store = tmp_dir + "/sonic-pi-pids"

unless File.exists? pids_store
  log_process_info "Creating pids store: #{pids_store}"
  Dir.mkdir(pids_store)
end

pid = ARGV[0].to_i
pid_path = "#{pids_store}/#{pid}"

f = nil

begin
  if s = Sys::ProcTable.ps(pid: pid)
    f = File.open(pid_path, 'w')
    f.puts s.cmdline
    log_process_info "Started [#{pid}] [-] #{s.cmdline} [-] #{pid_path}"
  end
rescue Exception => e
  log_process_info "ERROR: Unable to write information for PID #{pid} to path #{pid_path}!"
  log_process_info "#{e}"
end

f.close if f
