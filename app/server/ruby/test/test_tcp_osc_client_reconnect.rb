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

# Spider's one connection to the engine carries every subsystem and the
# relay to the GUI, so what the client does around a reconnect, and with a
# handler that fails, is everyone's problem. Against a fake engine.

require 'minitest/autorun'
require 'timeout'
require_relative '../lib/sonicpi/osc/tcp_osc_client'
require_relative '../lib/sonicpi/promise'
require_relative './lib/fake_engine'

module SonicPi
  class TcpOscClientReconnectTester < Minitest::Test
    def setup
      @engine = FakeEngine.new
    end

    def teardown
      @client&.stop
      @engine.stop
    end

    def connect
      @client = OSC::TcpOscClient.new('127.0.0.1', @engine.port, name: 'reconnect-test', connect_timeout: 5)
      @engine.await_connection
    end

    def now
      Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    # ── a reconnect ───────────────────────────────────────────────────────

    # After a reconnect the owning layers register with the engine again and
    # wait for its answer (register_for_notifications!, 5 s): the reply has
    # to reach them while they wait, or every reconnect costs the wait and
    # nothing from the engine is heard until it is up.
    def test_a_reconnect_hook_hears_the_reply_it_waits_for
      first = connect
      @engine.on("/ping") { |args| ["/pong", *args] }
      outcome = Promise.new
      @client.add_method("/pong") { |args| @pong&.deliver!(args) unless @pong&.delivered? }
      @client.on_reconnect do
        @pong = Promise.new
        started = now
        @client.send(nil, nil, "/ping", 7)
        begin
          outcome.deliver!([:replied, @pong.get(3), now - started])
        rescue StandardError
          outcome.deliver!([:timed_out, nil, now - started])
        end
      end

      @engine.drop(first)
      @engine.await_connection
      kind, args, took = outcome.get(10)
      assert_equal :replied, kind, "the hook waited #{took.round(2)} s and never heard /pong"
      assert_equal [7], args
      assert_operator took, :<, 1.0, "the reply took #{took.round(2)} s to reach the hook"
    end

    def test_reconnect_hooks_run_in_the_order_they_were_registered
      first = connect
      order = Queue.new
      @client.on_reconnect { order << :scsynth }
      @client.on_reconnect { order << :midi }
      @client.on_reconnect { order << :link }
      @engine.drop(first)
      @engine.await_connection
      ran = 3.times.map { Timeout.timeout(5) { order.pop } }
      assert_equal %i[scsynth midi link], ran
    end

    # ── a handler that fails ──────────────────────────────────────────────

    # Several layers hear the same address: the owner's handler, the relay's
    # listeners, the global matcher. One that fails (the relay's send to a
    # GUI that has gone, say) must not take the others down with it: the
    # rebuild is started from a listener on /clockwork/setup.
    def test_a_listener_that_raises_does_not_silence_the_others
      connect
      heard = Queue.new
      @client.add_method("/clockwork/setup") { |args| heard << [:handler, args] }
      @client.add_listener("/clockwork/setup") { |_| raise Errno::ECONNREFUSED, "the GUI has gone" }
      @client.add_listener("/clockwork/setup") { |args| heard << [:rebuild, args] }
      @client.add_global_method { |address, args, _| heard << [:global, address] }

      @engine.send(1, "/clockwork/setup", 48_000, 512, 3)
      got = 3.times.map { Timeout.timeout(3) { heard.pop } rescue :nothing }
      assert_includes got, [:handler, [48_000, 512, 3]]
      assert_includes got, [:rebuild, [48_000, 512, 3]], "the listener after the one that failed never ran"
      assert_includes got, [:global, "/clockwork/setup"], "the global matcher never ran"
    end

    def test_a_handler_that_raises_does_not_silence_the_listeners
      connect
      heard = Queue.new
      @client.add_method("/clockwork/info") { |_| raise "the owner's handler failed" }
      @client.add_listener("/clockwork/info") { |args| heard << [:listener, args] }
      @client.add_global_method { |address, _, _| heard << [:global, address] }

      @engine.send(1, "/clockwork/info", "hello")
      got = 2.times.map { Timeout.timeout(3) { heard.pop } rescue :nothing }
      assert_includes got, [:listener, ["hello"]], "the listener never ran"
      assert_includes got, [:global, "/clockwork/info"], "the global matcher never ran"
    end

    def test_a_handler_that_raises_does_not_cost_the_connection_or_the_next_message
      connect
      heard = Queue.new
      @client.add_method("/a") { |_| raise "failed" }
      @client.add_method("/b") { |args| heard << args }
      @engine.send(1, "/a")
      @engine.send(1, "/b", 1)
      assert_equal [1], Timeout.timeout(3) { heard.pop }
      assert @client.connected?
    end
  end
end
