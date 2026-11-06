#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "osc/tcp_osc_client"

module SonicPi
  # Spider's one connection to SuperSonic. The engine serves one client on
  # its command port, so the scsynth link (SCSynthExternal) and the MIDI,
  # gamepad, Link and OSC comms (SupersonicComms) all send and listen on the
  # TcpOscClient this hands out, made on the first ask with the secret the
  # daemon gave Spider (EngineSecret). Each caller adds its own handlers and
  # reconnect hooks to it; nothing but the process's exit stops it.
  module EngineConnection
    @mutex = Mutex.new
    @clients = {}
    @secret = nil

    class << self
      attr_accessor :secret

      def client(host, port, name: "Engine", connect_timeout: 60)
        key = "#{host}:#{Integer(port)}"
        @mutex.synchronize do
          @clients[key] ||= OSC::TcpOscClient.new(host, port,
                                                 use_decoder_cache: true,
                                                 use_encoder_cache: true,
                                                 name: name,
                                                 connect_timeout: connect_timeout,
                                                 secret: @secret)
        end
      end

      # Tests: drop every client so the next ask connects afresh.
      def reset!
        @mutex.synchronize do
          @clients.each_value { |c| c.stop rescue nil }
          @clients.clear
          @secret = nil
        end
      end
    end
  end
end
