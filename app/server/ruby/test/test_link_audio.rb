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

require_relative "./setup_test"
require_relative "../lib/sonicpi/studio"
require_relative "../lib/sonicpi/link_api"
require_relative "../lib/sonicpi/supersonic_link_comms"
require_relative "./lib/headless_engine"

module SonicPi
  # Regression guarded against: link_audio was silent between two machines.
  # Clockwork takes a subscription's channel as one of its input channels,
  # while Sonic Pi still sent one of its own private buses, so the engine
  # refused every subscription and nothing said so. Sonic Pi's other tests
  # stand a mock in for the Link API, so none of them saw what reached the
  # engine.
  #
  # Two headless engines on this machine, Link at loopback visibility: one
  # publishes its output as "Main", the other is Sonic Pi's, and link_audio's
  # subscription goes from the second to the first. Skipped when the engine is
  # not built, as the mix tests are.
  class LinkAudioTester < Minitest::Test
    PEER = "Sonic Pi link_audio test peer"
    LOOPBACK = 1   # /clockwork/clock/visibility: this machine only

    def setup
      skip "engine not built at #{MixEngine::ENGINE}" unless HeadlessEngine.available?
      @peer = HeadlessEngine.new("link-audio-peer")
      @peer_comms = SupersonicLinkComms.new("127.0.0.1", @peer.port)
      @peer_comms.send("/clockwork/clock/peer_name/set", PEER)
      @peer_comms.send("/clockwork/clock/visibility", LOOPBACK)
      @peer_comms.send("/clockwork/clock/audio/publish/set", 1)

      @engine = HeadlessEngine.new("link-audio")
      @studio = Studio.new({scsynth_port: @engine.port, scsynth_send_port: @engine.port}, Queue.new,
                           HeadlessEngine::State.new, ->(*) {}, -> { nil })
      @link = LinkAPI.new("127.0.0.1", @engine.port, {}, clock_reader: nil)
      @link.link_set_visibility!(LOOPBACK)
      @probe = Probe.new(@engine.port)
      assert eventually(30) { channel_visible?(PEER, "Main") }, "the peer's Main channel never appeared"
    end

    def teardown
      unless passed? || @engine.nil?
        puts "\n--- #{name}: Sonic Pi's engine's log, #{@engine.log} ---", @engine.log_tail
        puts "\n--- the peer's log, #{@peer.log} ---", @peer.log_tail
      end
      @studio&.server&.shutdown rescue nil
      @engine&.stop
      @peer&.stop
    end

    def test_link_audio_takes_a_peers_channel_on_the_bus_it_arrives_on
      in_bus = @studio.ensure_link_audio_input(PEER, "Main", @link)
      input = eventually(10) { input(PEER, "Main") }
      refute_nil input, "the engine did not take the subscription"
      assert_equal @studio.scsynth_info[:num_output_busses] + input[:channel], in_bus,
                   "link_audio's synth would read a different bus from the one the stream arrives on"
      assert eventually(10) { input(PEER, "Main")&.fetch(:state) == CONNECTED }, "the stream never connected"
    end

    private

    CONNECTED = 2   # LinkAudioBridge's ConnectionState::Connected

    def eventually(seconds)
      deadline = Time.now + seconds
      loop do
        result = yield
        return result if result || Time.now > deadline
        Kernel.sleep 0.1
      end
    end

    # /clockwork/clock/audio/channels.reply: count, then per channel
    # [id name peer_id peer_name].
    def channel_visible?(peer, channel)
      reply = @probe.ask("/clockwork/clock/audio/channels/get", "/clockwork/clock/audio/channels.reply")
      return false unless reply
      reply.drop(1).each_slice(4).any? { |_id, name, _peer_id, peer_name| name == channel && peer_name == peer }
    end

    # /clockwork/clock/audio/inputs.reply: count, then per input twelve fields,
    # [peer channel input_channel sample_rate source_channels buffered_ms state
    # ...].
    def input(peer, channel)
      reply = @probe.ask("/clockwork/clock/audio/inputs/get", "/clockwork/clock/audio/inputs.reply")
      return nil unless reply
      entry = reply.drop(1).each_slice(12).find { |p, c, *| p == peer && c == channel }
      entry && { channel: entry[2], state: entry[6] }
    end

    # Asks without a correlation token, so the answer is the engine's own
    # whether or not that reply echoes one.
    class Probe
      def initialize(port)
        @port = port
        @replies = Queue.new
        @heard = {}
        @client = OSC::TcpOscClient.new("127.0.0.1", port, name: "link_audio test probe")
      end

      def ask(address, reply_address, timeout = 1)
        @heard[reply_address] ||= @client.add_method(reply_address) { |args| @replies << args } || true
        @replies.clear
        @client.send("127.0.0.1", @port, address)
        Timeout.timeout(timeout) { @replies.pop }
      rescue Timeout::Error
        nil
      end
    end
  end
end
