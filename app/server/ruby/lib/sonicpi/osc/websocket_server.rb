#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2018 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'socket'
require_relative "../util"
require 'rubame'

# monkey patch rubame to add the ability to send binary
# data down the websocket
module Rubame
  class Client
    def __sonic_pi_send_binary(data)
      frame = WebSocket::Frame::Outgoing::Server.new(:version => @handshake.version, :data => data, :type => :binary)
      begin
        @socket.write frame
        @socket.flush
      rescue
        @server.close(self) unless @closed
      end
    end
  end
end

module SonicPi
  module OSC
    class WebSocketServer
      include Util

      def initialize(port, opts={})
        @port = port
        @opts = opts
        @server = Rubame::Server.new("localhost", port)
        @client = nil
        @matchers = {}
        @decoder = FastOsc
        @encoder = FastOsc
        @listener_thread = Thread.new {start_listener}
        @connected = false
      end

      def send(pattern, *args)
        log "websocket outgoing: #{pattern}"
        msg = @encoder.encode_single_message(pattern, args)

        # TODO - this is just temporary to stop the initial messages
        # falling into the ether before a client has connected.
        # Need to properly consider this behaviour and appropriately
        # improve.
        while ! @client
          Kernel.sleep 0.1
        end
        @client.__sonic_pi_send_binary(msg)
      end

      def send_ts(ts, pattern, *args)
        msg = @encoder.encode_single_bundle(ts, pattern, args)
        @client.__sonic_pi_send_binary(msg)
      end

      def add_method(address_pattern, &proc)
        @matchers[address_pattern] = proc
      end

      def to_s
        "#<SonicPi::OSC::WebSocketServer port: #{@port}, opts: #{@opts.inspect}>"
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
          @server.run do |client|

            @client = client
            client.onopen do
              log "WebSocketServer reports:  client open #{client}"
            end

            client.onmessage do |mess|
              begin
              o = @decoder.decode_single_message(mess)
                log "websocket incoming: #{o[0]}, #{o[1]}"
              p = @matchers[o[0]]
              p.call(o[1]) if p
              rescue Exception => e
                STDERR.puts "Websocket-based OSC handler exception for address: #{address}"
                STDERR.puts e.message
                STDERR.puts e.backtrace.inspect
              end
            end

            client.onclose do
              log "WebSocketServer reports:  client closed  #{client}"
            end
          end
        end
      end
    end
  end
end
