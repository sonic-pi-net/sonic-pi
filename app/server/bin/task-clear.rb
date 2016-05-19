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
tmp_dir = Dir.tmpdir

#f = File.open("log_path/spawn.log", 'a')

pids_store = tmp_dir + "/sonic-pi-pids"


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

pids = Dir.entries(pids_store) - [".", ".."]
# f.puts "clearning"
# f.puts "Found pids: #{pids}"

pids.each do |pid|
  # We're on Windows, so go straight for the jugular
  # f.puts "clearing pid: #{pid}"
  if os == :windows
    begin
      Process.kill(9, pid)
      # puts "Killed #{pid}"
    rescue Exception => e
      # puts "Could not kill #{pid} - perhaps already killed?"
    end
  else

    # puts "going to kill #{pid}"
    pid = Integer(pid)
    begin
      Process.kill(15, pid)
      15.to_i.times do
        # f.puts 'trying to kill pid'
        begin
          alive = Process.waitpid(pid, Process::WNOHANG)
          unless alive
            # f.puts "Successfully killed #{pid}"
            break
          end
        rescue Exception => e
          # process is definitely dead!
          # puts "Error waiting for process #{pid} - assumed already killed"
          break
        end
        sleep 1
      end

      Process.kill(9, pid)
      # f.puts "Forcibly killed #{pid}"
    rescue Errno::ECHILD => e
      # f.puts "Unable to wait for #{pid} - child process does not exist"
    rescue Errno::ESRCH
      # f.puts "Unable to kill #{pid} - process does not exist"
    end
  end
  pid_path = "#{pids_store}/#{pid}"
  FileUtils.rm pid_path if File.exists? pid_path
end

# f.puts "Finished clearing pids"
# f.close
