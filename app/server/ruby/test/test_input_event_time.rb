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
require_relative "./setup_test"
require_relative "../lib/sonicpi/osc/osc"
require_relative "../lib/sonicpi/midi_api"
require_relative "../lib/sonicpi/gamepad_api"

module SonicPi
  # Every MIDI and gamepad event from the engine ends in a timetag: the moment
  # it arrived. A cue carries the event's fields, not that time, as the web
  # does (app/web/web/live-worker.js).
  class InputEventTimeTester < Minitest::Test
    # A comms that keeps the handlers it is given, so a test can deliver to
    # them without a socket.
    class FakeComms
      attr_reader :handlers
      def initialize
        @handlers = {}
      end

      def add_method(address, &handler)
        @handlers[address] = handler
      end
    end

    ARRIVED = ::SonicPi::OSC::TimeTag.new(0xE000_0000_8000_0000)

    def api_with_fake_comms(klass, comms_ivar)
      cues = []
      api = klass.allocate
      comms = FakeComms.new
      api.instance_variable_set(comms_ivar, comms)
      api.instance_variable_set(:@internal_cue_handler, ->(path, args) { cues << [path, args] })
      [api, comms, cues]
    end

    def test_a_midi_event_cues_its_fields_without_its_arrival_time
      api, comms, cues = api_with_fake_comms(MidiAPI, :@midi_comms)
      api.instance_variable_set(:@disabled_ports, { in: [], out: [] })
      api.send(:add_supersonic_midi_handlers!)

      sysex = "\xF0\x7E\xF7".b
      comms.handlers["/clockwork/midi/in/note_on"].call(["keys", 1, 60, 100, ARRIVED])
      comms.handlers["/clockwork/midi/in/sysex"].call(["keys", sysex, ARRIVED])
      comms.handlers["/clockwork/midi/in/start"].call(["keys", ARRIVED])

      assert_equal([["/midi:keys:1/note_on", [60, 100]],
                    ["/midi:keys/sysex", [sysex]],
                    ["/midi:keys/start", []]], cues)
    end

    def test_a_gamepad_event_cues_its_fields_without_its_arrival_time
      api, comms, cues = api_with_fake_comms(GamepadAPI, :@gamepad_comms)
      api.instance_variable_set(:@pressed, {})
      api.send(:add_supersonic_gamepad_handlers!)

      comms.handlers["/clockwork/gamepad/in/axis"].call(["pad", "left_x", 0.5, ARRIVED])
      comms.handlers["/clockwork/gamepad/in/button"].call(["pad", "south", 1, 1.0, ARRIVED])

      assert_includes(cues, ["/gamepad:pad/axis/left_x", [0.5]])
      assert_includes(cues, ["/gamepad:pad/button/south", [1, 1.0]])
    end
  end
end
