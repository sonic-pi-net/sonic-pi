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
    class UDPClient

      def initialize(host, port, opts={})
        @host = host
        @port = port
        @opts = opts
        use_encoder_cache = opts[:use_encoder_cache]
        encoder_cache_size = opts[:encoder_cache_size] || 1000
        @encoder = OscEncode.new(use_encoder_cache, encoder_cache_size)
        @so = UDPSocket.new
        @so.connect(host, port)
      end

      def send(pattern, *args)
        msg = @encoder.encode_single_message(pattern, args)
        @so.send(msg, 0)
      end

      def send_ts(ts, pattern, *args)
        msg = @encoder.encode_single_bundle(ts, pattern, args)
        @so.send(msg, 0)
      end

      def to_s
        "#<SonicPi::OSC::UDPClient host: #{@host}, port: #{@port}, opts: #{@opts.inspect}>"
      end

      def inspect
        to_s
      end

    end
  end
end
