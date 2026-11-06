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

# The engine answers "/clockwork/error [address, reason, generation]" to
# whoever sent what it refused, and Spider's connection now carries the
# GUI's and the daemon's messages too. What SCSynthExternal makes of a
# refusal, whoever's message it was: against a fake engine.

require_relative "./setup_test"
require_relative "../lib/sonicpi/scsynthexternal"
require_relative "../lib/sonicpi/engine_connection"
require_relative "./lib/fake_engine"

module SonicPi
  class SCSynthExternalRefusalsTester < Minitest::Test
    # What the engine's handler hands the runtime.
    class Events
      attr_reader :seen
      def initialize; @seen = Queue.new; end
      def async_event(address, args); @seen << [address, args]; end
    end

    def setup
      EngineConnection.reset!
      @engine = FakeEngine.new
      # The reply, then the state the engine replays to a new registrant.
      @engine.on("/clockwork/notify") { |_| [["/clockwork/notify.reply", 1, "0.95.1"], ["/clockwork/statechange", "running", ""]] }
      @events = Events.new
      @external = SCSynthExternal.new(@events, @engine.port, ->(*) {})
      @refusals = Queue.new
      @external.on_refusal = ->(address, reason, generation) { @refusals << [address, reason, generation] }
    end

    def teardown
      @engine.stop
      EngineConnection.reset!
    end

    def refusal(timeout = 2)
      Timeout.timeout(timeout) { @refusals.pop }
    rescue Timeout::Error
      :none
    end

    def test_registers_for_notifications_and_takes_the_version_from_the_reply
      assert_equal "v0.95.1", @external.version
      assert_equal "/clockwork/notify", @engine.next_received[1]
    end

    # Between instances is the engine's state, not a fact about one sender:
    # a refusal of the GUI's device switch for "rebuilding" says the setup
    # is coming just as a refusal of our own would.
    def test_a_refusal_for_between_instances_is_heard_whoever_sent_the_message
      @engine.send(1, "/clockwork/error", "/clockwork/devices/switch", "rebuilding", 9)
      assert_equal ["/clockwork/devices/switch", "rebuilding", 9], refusal
    end

    def test_each_of_the_three_between_instances_reasons_is_heard
      %w[rebuilding no\ instance stalled].each_with_index do |reason, i|
        @engine.send(1, "/clockwork/error", "/sync", reason, i)
        assert_equal ["/sync", reason, i], refusal, reason
      end
    end

    # A verb the engine does not know, or an argument it will not take, is
    # a mistake of ours, not a wait for a setup that is not coming.
    def test_any_other_refusal_is_not_a_wait_for_setup
      @engine.send(1, "/clockwork/error", "/clockwork/track/gain", "unknown command", 9)
      assert_equal :none, refusal
    end

    def test_every_refusal_still_reaches_the_runtime_as_an_event
      @engine.send(1, "/clockwork/error", "/clockwork/track/gain", "unknown command", 9)
      @engine.send(1, "/clockwork/error", "/sync", "rebuilding", 9)
      seen = []
      Timeout.timeout(3) { seen << @events.seen.pop until seen.count { |a, _| a == "/clockwork/error" } == 2 }
      seen.reject! { |a, _| a != "/clockwork/error" }   # the registration's own reply and state came first
      assert_equal [["/clockwork/error", ["/clockwork/track/gain", "unknown command", 9]],
                    ["/clockwork/error", ["/sync", "rebuilding", 9]]], seen
    end
  end
end
