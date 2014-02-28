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

require_relative "core.rb"

require_relative "sonicpi/scsynth"
require_relative "sonicpi/studio"
require_relative "sonicpi/spider"
require_relative "sonicpi/server"
require_relative "sonicpi/util"
require_relative "sonicpi/rcv_dispatch"

Thread.abort_on_exception=true

include SonicPi::Util

ws_out = Queue.new
$scsynth = SonicPi::SCSynth.instance

$c = OSC::Client.new("localhost", 4556)

at_exit do
  $c.send(OSC::Message.new("/quit"))
end

$c.send(OSC::Message.new("/d_loadDir", synthdef_path))
sleep 2

$sp =  SonicPi::Spider.new "localhost", 4556, ws_out, 5

$rd = SonicPi::RcvDispatch.new($sp, ws_out)
$clients = []

# Send stuff out from Sonic Pi jobs out to GUI
out_t = Thread.new do
  continue = true
  while continue
    begin
      message = ws_out.pop
      message[:ts] = Time.now.strftime("%H:%M:%S")

      if message[:type] == :exit
        continue = false
      else
        puts message
      end
    rescue Exception => e
      puts "Exception!"
      puts e.message
      puts e.backtrace.inspect
    end
  end
end

$rd.dispatch({:cmd => "run-code",
              :val => ARGF.read})

out_t.join
