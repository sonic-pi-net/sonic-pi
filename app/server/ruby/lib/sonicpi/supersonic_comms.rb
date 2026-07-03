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
  # OSC client for one SuperSonic subsystem (an address space such as /midi,
  # /clock or /gamepad). send is fire-and-forget; rpc blocks for a reply at
  # the expect-address and returns its args (nil on timeout);
  # subscribe_to_notifications! registers this client for the subsystem's
  # pushes and must run once after the add_method handlers are wired.
  class SupersonicComms
    def initialize(supersonic_host, supersonic_port, address_space:, name:)
      @host = supersonic_host.freeze
      @port = Integer(supersonic_port)
      @address_space = address_space.freeze
      @udp_server = SonicPi::OSC::UDPServer.new(0, name: name)
      # Each rpc request carries a correlation token as its last int32
      # argument, which SuperSonic echoes as the last argument of the reply
      # (see "Correlation tokens" in SuperSonic's docs/OSC_API.md). A reply
      # whose token does not match the outstanding request is discarded.
      # Without it, a reply that arrives after its caller has timed out would
      # be delivered to the next caller on the same address, giving it a
      # stale answer. rpc() also serialises per address (@rpc_locks) so only
      # one request is outstanding at a time.
      @reply_mut = Mutex.new
      @rpc_locks = Hash.new { |h, k| h[k] = Mutex.new }
      @pending = {}
      @rpc_token = 0
      @reply_handlers_installed = {}
    end

    def send(pattern, *args)
      @udp_server.send(@host, @port, pattern, *args)
    end

    def rpc(req_pattern, *args, expect:, timeout: 1.0)
      # Serialise per expect-address (see initialize). Racing callers queue
      # on the lock; these RPCs are sub-millisecond on loopback, so the
      # serialisation cost is negligible next to unambiguous reply matching.
      @rpc_locks[expect].synchronize do
        promise = Promise.new
        token = nil
        @reply_mut.synchronize do
          token = @rpc_token = (@rpc_token + 1) & 0x7FFFFFFF
          # Install the per-address reply dispatcher once.
          unless @reply_handlers_installed[expect]
            @reply_handlers_installed[expect] = true
            @udp_server.add_method(expect) do |reply_args|
              matched = @reply_mut.synchronize do
                entry = @pending[expect]
                if entry && reply_args.last == entry[0]
                  @pending.delete(expect)
                else
                  # No outstanding request, or a token from an already-
                  # abandoned one: a stale reply. Drop it; the live
                  # request's reply (with the right token) may still come.
                  nil
                end
              end
              if matched
                p = matched[1]
                # Strip the echoed token — callers see the verb's own args.
                p.deliver!(reply_args[0...-1]) unless p.delivered?
              end
            end
          end
          @pending[expect] = [token, promise]
        end
        send(req_pattern, *args, token)
        begin
          promise.get(timeout)
        rescue Exception
          @reply_mut.synchronize do
            entry = @pending[expect]
            @pending.delete(expect) if entry && entry[1].equal?(promise)
          end
          nil
        end
      end
    end

    def add_method(pattern, &blk)
      @udp_server.add_method(pattern, &blk)
    end

    def subscribe_to_notifications!
      send("#{@address_space}/notify/subscribe")
    end

    def unsubscribe_from_notifications!
      send("#{@address_space}/notify/unsubscribe")
    end
  end
end
