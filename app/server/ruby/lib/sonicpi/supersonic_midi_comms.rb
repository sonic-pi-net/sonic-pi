#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "promise"
require_relative "osc/udp_server"

module SonicPi
  # OSC client for SuperSonic's MIDI subsystem, over the /midi/* address space.
  # A sibling of SupersonicLinkComms (same shape as the Ableton Link move):
  # send is fire-and-forget; rpc blocks for a reply; subscribe_to_notifications!
  # registers this client for /midi/in/* + /midi/ports pushes.
  class SupersonicMidiComms
    def initialize(supersonic_host, supersonic_port)
      @host = supersonic_host.freeze
      @port = Integer(supersonic_port)
      @udp_server = SonicPi::OSC::UDPServer.new(0,
                                                name: "SuperSonic MIDI Comms")
      @reply_mut = Mutex.new
      @reply_queues = Hash.new { |h, k| h[k] = [] }
      @reply_handlers_installed = {}
    end

    # The shared OSC encoder, used to build the inner /midi/out blob carried by
    # /midi/at.
    def encoder
      @udp_server.encoder
    end

    def send(pattern, *args)
      @udp_server.send(@host, @port, pattern, *args)
    end

    def rpc(req_pattern, *args, expect:, timeout: 1.0)
      promise = Promise.new
      @reply_mut.synchronize do
        unless @reply_handlers_installed[expect]
          @reply_handlers_installed[expect] = true
          @udp_server.add_method(expect) do |reply_args|
            next_p = @reply_mut.synchronize { @reply_queues[expect].shift }
            next_p.deliver!(reply_args) if next_p && !next_p.delivered?
          end
        end
        @reply_queues[expect] << promise
      end
      send(req_pattern, *args)
      begin
        promise.get(timeout)
      rescue Exception
        @reply_mut.synchronize { @reply_queues[expect].delete(promise) }
        nil
      end
    end

    def add_method(pattern, &blk)
      @udp_server.add_method(pattern, &blk)
    end

    def subscribe_to_notifications!
      send("/midi/notify/subscribe")
    end

    def unsubscribe_from_notifications!
      send("/midi/notify/unsubscribe")
    end
  end
end
