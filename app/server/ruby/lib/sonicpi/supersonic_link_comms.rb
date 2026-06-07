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
  # OSC client for SuperSonic's Link API, over the /clock/* address space.
  # send is fire-and-forget; rpc blocks for a reply at the expect-address
  # and returns its args (nil on timeout). subscribe_to_notifications!
  # must run once after the /clock/notify/* add_method handlers are wired.
  class SupersonicLinkComms
    def initialize(supersonic_host, supersonic_port)
      @host = supersonic_host.freeze
      @port = Integer(supersonic_port)
      @udp_server = SonicPi::OSC::UDPServer.new(0,
                                                name: "SuperSonic Link Comms")
      # FIFO queue of outstanding promises per expect-address. SuperSonic
      # replies to /clock/rpc/* in request order, so the next reply belongs
      # to the head promise even when live_loops race the same RPC.
      @reply_mut = Mutex.new
      @reply_queues = Hash.new { |h, k| h[k] = [] }
      @reply_handlers_installed = {}
    end

    def send(pattern, *args)
      @udp_server.send(@host, @port, pattern, *args)
    end

    def rpc(req_pattern, *args, expect:, timeout: 1.0)
      promise = Promise.new
      @reply_mut.synchronize do
        # Install the per-address reply dispatcher once.
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
        # Drop our still-pending promise so the next reply doesn't fill it.
        @reply_mut.synchronize do
          @reply_queues[expect].delete(promise)
        end
        nil
      end
    end

    def add_method(pattern, &blk)
      @udp_server.add_method(pattern, &blk)
    end

    def subscribe_to_notifications!
      send("/clock/notify/subscribe")
    end

    def unsubscribe_from_notifications!
      send("/clock/notify/unsubscribe")
    end
  end
end
