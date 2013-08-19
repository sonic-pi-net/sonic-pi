#!/usr/bin/env ruby
$:.unshift File.expand_path("../vendor/osc-ruby/lib", __FILE__)
load(File.absolute_path("#{File.dirname(__FILE__)}/util.rb"))

require 'osc-ruby'

spider_log ""
spider_log ""
spider_log "Booting Sonic Pi"
spider_log "----------------"
spider_log ""

#start dbus
`eval $(dbus-launch --auto-syntax)`
sleep 2

#Kill any existing services that belong to us
kill_scsynth
kill_jackd

#Start Jack if not already running
if `ps cax | grep jackd`.empty?
  #Jack not running - start a new instance and store its PID
  spider_log "Jackd not running on system. Starting..."
  system("jackd -R -p 32 -d alsa -n 3&y ")
  sleep 3
  jack_pid = `ps cax | grep jackd`.split(" ").first
  spider_log "Jack started with pid #{jack_pid}"
  write_jackd_pid(jack_pid)
else
  spider_log "Jackd already running. Not starting another server..."
end

#Start new instance of SuperCollider server and store its PID.
existing_scsynth_pids  = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
spider_log "Starting the SuperCollider server..."
system("scsynth -u 4556 -m 131072 &")
sleep 4
updated_scsynth_pids  = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
scsynth_pid = (updated_scsynth_pids - existing_scsynth_pids).first
write_scsynth_pid(scsynth_pid)

# hook up jack to supercollider
spider_log "Connecting SuperCollider to the System Audio via Jack..."
`jack_connect SuperCollider:out_1 system:playback_1`
`jack_connect SuperCollider:out_2 system:playback_2`

# ensure that the synthdefs have been loaded
client = OSC::Client.new('localhost', 4556)
client.send(OSC::Message.new("/d_loadDir", sp_synthdefs_path))
