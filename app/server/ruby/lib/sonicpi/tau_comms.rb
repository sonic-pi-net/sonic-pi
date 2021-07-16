#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2021 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "util"
require_relative "promise"
require_relative "osc/udp_server"

module SonicPi
  class TauComms
    include Util

    attr_reader :encoder

    def initialize(hostname, port)
      @hostname = hostname
      @port = port
      @udp_client = SonicPi::OSC::UDPClient.new("127.0.0.1", port)
      @encoder = @udp_client.encoder
      @mut = Mutex.new
      @tau_is_ready = false
      @buffered_msgs = []
      @wait_for_tau_thr = Thread.new do
        wait_for_tau!
        @mut.synchronize do
          # flush stored messages and send them directly
          @buffered_msgs.each do |msg|
            if msg[0] == :send
              @udp_client.send(msg[1], *msg[2])
            elsif msg[0] == :send_ts
              @udp_client.send_ts(msg[1], msg[2], *msg[3])
            else
              raise StandardError "Unknown TauComms msg type: #{msg[0]}. Expecting one of :send or :send_ts"
            end
          end
          @buffered_msgs.clear
          @tau_is_ready = true
        end
      end
    end



    def wait_for_tau!
      p = Promise.new

      booted = false
      connected = false

      boot_s = OSC::UDPServer.new(0) do |a, b, info|
        log "TauComms - Receiving ack from tau"
        p.deliver! true unless connected
        connected = true
      end

      t = Thread.new do
        Kernel.loop do
          begin
            log "Boot - Sending /ping to tau: #{@hostname}:#{@port}"
            boot_s.send(@hostname, @port, "/ping")
          rescue Exception => e
            log "Boot - Error sending /ping to tau: #{e.message}"
          end
          sleep 1
        end
      end

      begin
        p.get(30)
      rescue Exception => e
        log "TauComms - Unable to connect to tau. Exiting..."
        exit
      ensure
        t.kill
      end

      unless connected
        log "TauComms - Unable to connect to tau"
        exit
      end

      log "TauComms - connection established"

    end

    def send(pattern, *args)
      if @tau_is_ready
        @udp_client.send(pattern, *args)
      else
        @mut.synchronize do
          if @tau_is_ready
            @udp_client.send(pattern, *args)
          else
            @buffered_msgs << [:send, pattern, args]
          end
        end
      end
    end

    def send_ts(ts, pattern, *args)
      if @tau_is_ready
        @udp_client.send_ts(ts, pattern, *args)
      else
        @mut.synchronize do
          if @tau_is_ready
            @udp_client.send_ts(ts, pattern, *args)
          else
            @buffered_msgs << [:send_ts, ts, pattern, args]
          end
        end
      end
    end
  end
end
