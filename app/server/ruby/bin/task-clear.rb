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

#f = File.open("log_path/spawn.log", 'a')
pids_store = tmp_dir + "/sonic-pi-pids"

unless File.exists? pids_store
  log_process_info "No pids store found here: #{pids_store}\n"
  log_process_info "Exiting\n"
  exit
end

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
       :unknown
     end

if ARGV.empty?
  pids = Dir.entries(pids_store) - [".", ".."]
else
  pids = ARGV
end

log_process_info "\n\nClearing pids: #{pids.inspect}\n"

if pids.empty?
  log_process_info "No pids to clear :-)\n"
  exit
end

pids.each do |pid|
  log_process_info "\nClearing [#{pid}]"
  pid = Integer(pid)
  pid_path = "#{pids_store}/#{pid}"
  begin
    orig_cmdline = File.readlines(pid_path)[0]
  rescue
    log_process_info "  -- unable to read original cmdline for pid: #{pid}"
    if File.exists? pid_path
      FileUtils.rm pid_path
    end
    next
  end

  log_process_info "  -- command #{orig_cmdline}"

  if File.exists? pid_path
    log_process_info "  -- removing #{pid_path}"
    FileUtils.rm pid_path
  end

  begin
    info = Sys::ProcTable.ps(pid: pid)
    raise unless info
  rescue
    log_process_info "  -- unable to get ProcTable info for: #{pid}"
    log_process_info "  -- process: #{pid} not running"
    next
  end

  # Don't kill process unless the command line arguments match
  next unless info.cmdline.strip == orig_cmdline.strip

  if os == :windows
    # We're on Windows, so go straight for the jugular
    log_process_info "  -- force killing #{pid}"
    begin
      Process.kill(9, pid)
      log_process_info "  -- killed #{pid}"
    rescue Exception => e
      log_process_info "  -- Could not kill #{pid} - perhaps already killed?"
    end
  else


    next if pid == 0
    cnt = 0
    begin
      8.times do
        if cnt < 3
          Process.kill(15, pid)
          log_process_info "  -- politely killing #{pid}"
        else
          Process.kill(9, pid)
          log_process_info "  -- force killing #{pid}"
        end
        sleep 0.5
        cnt += 1
      end

      log_process_info "  -- unable to kill #{pid}"
    rescue Errno::ESRCH => e
      if cnt == 0
        log_process_info "  -- process #{pid} already stopped"
      else
        log_process_info "  -- killed #{pid}"
      end
    rescue Exception => e
      log_process_info "  -- error killing process #{pid} - #{e.class}, #{e.message}\n#{e.backtrace.inspect}"
    end
  end


end

log_process_info "\nFinished clearing pids\n\n"
