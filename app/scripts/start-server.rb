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

require 'json'

include SonicPi::Util

ws_out = Queue.new
scsynth = SonicPi::SCSynth.instance
sc_server = OSC::Client.new("localhost", 4556)
osc_server = OSC::Server.new(4557)
proxy = OSC::Client.new("localhost", 4558)

at_exit do
  sc_server.send(OSC::Message.new("/quit"))
end

sp =  SonicPi::Spider.new "localhost", 4556, ws_out, 5
rd = SonicPi::RcvDispatch.new(sp, ws_out)

osc_server.add_method("/json") do |payload|
  puts "Received OSC: #{payload}"
  decoded = JSON.parse(payload.to_a[0])
  rd.dispatch(decoded)
end

Thread.new{osc_server.run}

# Send stuff out from Sonic Pi back out to osc_server
out_t = Thread.new do
  continue = true
  while continue
    begin
      message = ws_out.pop
      message[:ts] = Time.now.strftime("%H:%M:%S")

      if message[:type] == :exit
        continue = false
      else
        proxy.send(OSC::Message.new("/reply", JSON.fast_generate(message)))
      end
    rescue Exception => e
      puts "Exception!"
      puts e.message
      puts e.backtrace.inspect
    end
  end
end

out_t.join
