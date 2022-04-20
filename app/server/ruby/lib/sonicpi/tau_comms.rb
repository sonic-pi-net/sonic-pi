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

    def initialize(hostname, tau_port, listen_to_tau_port)
      @hostname = hostname.freeze
      @port = Integer(tau_port)
      @udp_server = SonicPi::OSC::UDPServer.new(listen_to_tau_port, name: "Tau Comms")
      @encoder = @udp_server.encoder
      @mut = Mutex.new
      @tau_ready = false
      @buffered_msgs = []
      @wait_for_tau_prom = Promise.new
      @wait_for_tau_thr = Thread.new do
        wait_for_tau!
        @wait_for_tau_prom.deliver!(true)
        @mut.synchronize do
          # flush stored messages and send them directly
          @buffered_msgs.each do |msg|
            if msg[0] == :send
              @udp_server.send(@hostname, @port, msg[1], *msg[2])
            elsif msg[0] == :send_ts
              @udp_server.send_ts(msg[1], @hostname, @port, msg[2], *msg[3])
            else
              raise StandardError "Unknown TauComms msg type: #{msg[0]}. Expecting one of :send or :send_ts"
            end
          end
          @buffered_msgs.clear
        end

        @tau_ready = true
      end
    end

    def send(pattern, *args)
      if @tau_ready
        @udp_server.send(@hostname, @port, pattern, *args)
      else
        @mut.synchronize do
          if @tau_ready
            @udp_server.send(@hostname, @port, pattern, *args)
          else
            @buffered_msgs << [:send, pattern, args]
          end
        end
      end
    end

    def send_ts(ts, pattern, *args)
      if @tau_ready
        @udp_server.send_ts(ts, @hostname, @port, pattern, *args)
      else
        @mut.synchronize do
          if @tau_ready
            @udp_server.send_ts(ts, @hostname, @port, pattern, *args)
          else
            @buffered_msgs << [:send_ts, ts, pattern, args]
          end
        end
      end
    end

    def add_method(address_pattern, &blk)
      @udp_server.add_method(address_pattern, &blk)
    end

    def add_global_method(&blk)
      @udp_server.add_global_method(&blk)
    end

    def block_until_tau_ready!
      return true if @tau_ready
      begin
        @wait_for_tau_prom.get(30)
      rescue Exception => e
        STDOUT.puts "TauComms - Unable to connect to tau (#{e.message}). Exiting..."
        exit
      end
    end

    def tau_ready?
      @tau_ready
    end

    private

    def wait_for_tau!
      p = Promise.new

      connected = false

      boot_s = OSC::UDPServer.new(0, name: "Tau Comms ack server") do |a, b, info|
        STDOUT.puts "TauComms - Receiving ack from tau"
        p.deliver! true unless connected
        connected = true
      end

      t = Thread.new do
        Kernel.loop do
          begin
            STDOUT.puts "TauComms - Sending /ping to tau: #{@hostname}:#{@port}"
            boot_s.send(@hostname, @port, "/ping")
          rescue Exception => e
            STDOUT.puts "TauComms - Error sending /ping to tau: #{e.message}"
          end
          sleep 1
        end
      end

      begin
        p.get(30)
      rescue Exception => e
        STDOUT.puts "TauComms - Unable to connect to tau (#{e.message}). Exiting..."
        exit
      ensure
        t.kill
      end

      unless connected
        STDOUT.puts "TauComms - Unable to connect to tau"
        exit
      end

      STDOUT.puts "TauComms - connection established"
    end
  end
end
