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

def sp_sonic_pi_path()
  File.absolute_path("#{File.dirname(__FILE__)}/../../")
end

def sp_scripts_path()
  File.absolute_path("#{sp_sonic_pi_path}/app/scripts")
end

def sp_synthdefs_path()
  File.absolute_path("#{sp_sonic_pi_path}/app/etc/synthdefs")
end

def spider_log(message)
  #File.open("/tmp/sonic-pi-spider.log", 'a') {|f| f.write("#{Time.now.strftime("%Y-%m-%d %H:%M:%S")} #{message}\n")}
end

def scsynth_pidfile()
  File.absolute_path("/tmp/sonic-pi-scsynth-pid")
end

def jackd_pidfile()
  File.absolute_path("/tmp/sonic-pi-jackd-pid")
end

def jackd_pid()
  if File.exists?(scsynth_pidfile)
    pid = File.readlines(jackd_pidfile).first.to_i
    pid == 0 ? nil : pid
  end
end

def scsynth_pid()
  if File.exists?(scsynth_pidfile)
    pid = File.readlines(scsynth_pidfile).first.to_i
    pid == 0 ? nil : pid
  end
end

def write_scsynth_pid(pid)
  File.open(scsynth_pidfile, 'w') { |file| file.write(pid) }
  spider_log("SCSynth PID [#{pid}] stored in file #{scsynth_pidfile}")
end

def write_jackd_pid(pid)
  File.open(jackd_pidfile, 'w') { |file| file.write(pid) }
  spider_log("Jackd PID [#{pid}] stored in file #{jackd_pidfile}")
end

def kill_scsynth()
  pid = scsynth_pid
  if pid
    `kill -9 #{pid}`
    File.open(scsynth_pidfile, 'w') { |file| file.write("") }
    spider_log "Killed scsynth with PID #{pid}"
  end
end

def kill_jackd()
  pid = jackd_pid
  if pid
    `kill -9 #{pid}`
    File.open(jackd_pidfile, 'w') { |file| file.write("") }
    spider_log "Killed jackd with PID #{pid}"
  end
end
