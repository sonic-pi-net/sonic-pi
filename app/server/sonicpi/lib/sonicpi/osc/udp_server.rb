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
require 'socket'

module SonicPi
  module OSC
    class UDPServer
      def initialize(port, opts={}, &proc)
        open = opts[:open]
        use_decoder_cache = opts[:use_decoder_cache]
        decoder_cache_size = opts[:decoder_cache_size] || 1000
        use_encoder_cache = opts[:use_encoder_cache]
        encoder_cache_size = opts[:encoder_cache_size] || 1000
        @port = port
        @opts = opts
        @socket = UDPSocket.new
        if open
          @socket.bind('', port )
        else
          @socket.bind('127.0.0.1', port )
        end
        @matchers = {}
        @global_matcher = proc
        @decoder = OscDecode.new(use_decoder_cache, decoder_cache_size)
        @encoder = OscEncode.new(use_encoder_cache, encoder_cache_size)
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
        loop do
          osc_data, network = @socket.recvfrom( 16384 )
          begin
            address, args = @decoder.decode_single_message(osc_data)
            if @global_matcher
              @global_matcher.call(address, args)
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
