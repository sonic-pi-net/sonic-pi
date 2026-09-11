## Unit tests for the Spider-side OscAPI wiring — the polarity of the cue-server
## prefs as they map onto the /osc/* schema. Guards against a boolean inversion
## in the GUI -> spider -> runtime -> osc_api chain (the enforcement of cues_on /
## loopback is tested in SuperSonic's Rust suite).

require_relative "../lib/sonicpi/osc_api"
require_relative "../lib/sonicpi/osc/osc_types"
require_relative "../lib/sonicpi/osc/oscencode"
require 'minitest'
require 'minitest/autorun'

module SonicPi
  class OscAPISemanticsTest < Minitest::Test
    # Captures what OscAPI would send to SuperSonic, without any socket.
    class RecordingComms
      attr_reader :sent
      def initialize; @sent = []; end
      def send(pattern, *args); @sent << [pattern, args]; end
      def encoder; @encoder ||= SonicPi::OSC::OscEncode.new; end
    end

    def make_api
      api = OscAPI.allocate           # skip #initialize (no real socket/subscribe)
      api.instance_variable_set(:@osc_comms, RecordingComms.new)
      api.instance_variable_set(:@global_timewarp, 0)
      api
    end

    def sent(api); api.instance_variable_get(:@osc_comms).sent; end

    # "Allow Incoming OSC": start (stop=false) enables forwarding, stop (=true)
    # disables it.
    def test_allow_incoming_osc_polarity
      api = make_api
      api.start_stop_cue_server!(false)   # enabled (GUI /cue-port-start)
      api.start_stop_cue_server!(true)    # disabled (GUI /cue-port-stop)
      assert_equal ["/clockwork/osc/cue-server/cues-on", [1]], sent(api)[0]
      assert_equal ["/clockwork/osc/cue-server/cues-on", [0]], sent(api)[1]
    end

    # "Allow OSC From Other Computers": internal=true is loopback-only, false is
    # all-interfaces.
    def test_allow_remote_osc_polarity
      api = make_api
      api.cue_server_internal!(true)      # loopback only (GUI /cue-port-internal)
      api.cue_server_internal!(false)     # all interfaces (GUI /cue-port-external)
      assert_equal ["/clockwork/osc/cue-server/loopback", [1]], sent(api)[0]
      assert_equal ["/clockwork/osc/cue-server/loopback", [0]], sent(api)[1]
    end

    # Flush on run-stop hits the shared scheduler flush.
    def test_osc_flush_uses_scheduler_flush
      api = make_api
      api.osc_flush!
      assert_equal ["/clockwork/sched/flush", ["default"]], sent(api).first
    end

    # Outgoing osc maps to /schedule <timetag> <blob: /osc/send <host> <port> <inner>>.
    # The host/port ride inside the self-routing /osc/send blob, not the wrapper, so
    # the scheduler re-ingests it through the same dispatch an immediate send hits.
    def test_send_osc_at_shape
      api = make_api
      api.send_osc_at(0.0, "127.0.0.1", 4560, "/foo", 1, "bar")
      pattern, args = sent(api).first
      assert_equal "/clockwork/schedule", pattern
      assert_kind_of SonicPi::OSC::Int64, args[0]
      assert_kind_of SonicPi::OSC::Blob, args[1]

      # The blob is a self-routing /osc/send carrying the destination + inner message.
      blob = args[1].to_s
      assert_includes blob, "/clockwork/osc/send"
      assert_includes blob, "127.0.0.1"
      assert_includes blob, "/foo"      # the user's inner address survives the wrap
    end
  end
end
