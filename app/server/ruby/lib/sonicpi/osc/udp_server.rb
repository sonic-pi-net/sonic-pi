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
require 'socket'
require_relative "../util"
require_relative "oscencode"
require_relative "oscdecode"

module SonicPi
  module OSC
    class UDPServer
      include Util

      def initialize(port, opts={}, &global_method)
        open = opts[:open]
        @port = port
        @opts = opts
        @socket = UDPSocket.new
        if open
          @socket.bind('', port )
        else
          @socket.bind('127.0.0.1', port )
        end
        @matchers = {}
        @global_matcher = global_method
        @decoder = OscDecode.new(true)
        @encoder = OscEncode.new(true)
        @listener_thread = Thread.new {start_listener}
      end

      def send(address, port, pattern, *args)
        msg = @encoder.encode_single_message(pattern, args)
        @socket.send(msg, 0, address, port)
      end

      def send_ts(ts, address, port, pattern, *args)
        msg = @encoder.encode_single_bundle(ts, pattern, args)
        @socket.send(msg, 0, address, port)
      end

      def add_method(address_pattern, &proc)
        @matchers[address_pattern] = proc
      end

      def add_global_method(&proc)
        @global_matcher = proc
      end

      def to_s
        "#<SonicPi::OSC::UDPServer port: #{@port}, opts: #{@opts.inspect}>"
      end

      def stop
        @listener_thread.kill
        @socket.close
      end

      def inspect
        to_s
      end

      private

      def start_listener
        Kernel.loop do
          begin
            osc_data, sender_addrinfo = @socket.recvfrom( 16384 )
          rescue Exception => e
            STDERR.puts "\n==========="
            STDERR.puts "Critical: UDP Server for port #{@port} had issues receiving from socket"
            STDERR.puts e.message
            STDERR.puts e.backtrace.inspect
            STDERR.puts "===========\n"
            redo
          end

          begin
            address, args = @decoder.decode_single_message(osc_data)
            log "OSC <-----        #{address} #{args.inspect}" if incoming_osc_debug_mode
            if @global_matcher
              @global_matcher.call(address, args, sender_addrinfo)
            else
              p = @matchers[address]
              p.call(args) if p
            end
          rescue Exception => e
            STDERR.puts "OSC handler exception for address: #{address}"
            STDERR.puts e.message
            STDERR.puts e.backtrace.inspect
          end
        end
      end
    end
  end
end
