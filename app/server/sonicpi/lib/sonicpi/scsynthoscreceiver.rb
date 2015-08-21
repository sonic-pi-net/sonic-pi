#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "util"
require_relative "oscdecode"

module SonicPi
  class ScsynthOSCReceiver
    include Util

    def initialize(port, osc_events)
      @socket = UDPSocket.new
      @socket.bind( '', port )
      @osc_events = osc_events
      @decoder = OscDecode.new(true)
    end

    def run
      @udp_incoming = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_osc_detector)
        Thread.current.priority = -10

        loop do
          osc_data, _network_ = @socket.recvfrom(16384)
          begin
            address, args = @decoder.decode_single_message(osc_data)
            handle_message(address, args)
            log "incoming: #{address}, #{args.inspect}" if incoming_osc_debug_mode
          rescue Exception => e
            log_exception e, "in detector"
          end
        end

      end
    end

    def send_raw(msg, address, port)
      @socket.send msg, 0, address, port
    end

    def stop
      @socket.close
      @udp_incoming.kill if @udp_incoming
    end

    def handle_message(address, args)
      case address
      when "/n_end"
        id = args[0].to_i
        @osc_events.async_event "/n_end/#{id}", args
      when "/n_off"
        id = args[0].to_i
        @osc_events.async_event "/n_off/#{id}", args
      when "/n_on"
        id = args[0].to_i
        @osc_events.async_event "/n_on/#{id}", args
      when "/n_go"
        id = args[0].to_i
        @osc_events.async_event "/n_go/#{id}", args
      else
        @osc_events.async_event address, args
      end
    end
  end
end
