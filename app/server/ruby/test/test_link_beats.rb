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
require_relative "../lib/sonicpi/link_api"
require_relative "../lib/sonicpi/osc/osc"

# A beat must cross the wire to the engine exactly.
#
# OSC 1.0 guarantees only four types (int32, float32, string, blob); float64 and
# int64 are optional extensions. Our encoder emits float32 for a Ruby Float, so
# a beat sent as a plain Float is quantised to a float32 — whose ulp is 2**-8
# beats once the session beat count passes 32768. At 60bpm that lands ~2ms of
# error on every beat->time conversion, i.e. on every sleep in clock bpm mode
# (runtime.rb:265) and on the phase-sync verbs.
#
# Beats therefore travel as int64 microbeats, which is Link's own internal
# representation (ableton/link/Beats.hpp) and matches how the sibling /clock
# verbs already carry time.

module SonicPi
  class LinkBeatsTester < Minitest::Test

    # A beat in the range where float32's ulp is 2**-8 (= 0.00390625) beats:
    # anything in [32768, 65536). At 60bpm one beat is one second, so that ulp
    # is 3.9ms of wall time and half-ulp is the ~2ms observed round-trip error.
    LARGE_BEAT = 40000.001

    def float32(v)
      [v].pack('g').unpack1('g')
    end

    def test_float32_cannot_carry_a_large_beat
      # Establishes the premise: this is what a plain Float costs on the wire.
      err = (float32(LARGE_BEAT) - LARGE_BEAT).abs
      assert err > 0.0009, "expected float32 to lose >0.9ms of beat, lost #{err}"
    end

    def test_microbeats_round_trip_exactly
      mb = LinkAPI.beats_to_microbeats(LARGE_BEAT)
      assert_kind_of Integer, mb
      assert_equal 40_000_001_000, mb
      # Back to beats within a microbeat — four orders below the float32 error.
      assert_in_delta LARGE_BEAT, LinkAPI.microbeats_to_beats(mb), 1e-6
    end

    def test_microbeats_preserve_a_millisecond_gap
      # Two beats 1ms apart at 60bpm. As float32 they snap to neighbouring
      # points on the 2**-8 grid, so the gap the engine sees is 0 or 3.9ms —
      # never the 1ms asked for. As microbeats the gap is exact.
      a = LARGE_BEAT
      b = LARGE_BEAT + 0.001
      f32_gap = float32(b) - float32(a)
      assert (f32_gap - 0.001).abs > 0.0009,
             "premise: float32 should distort a 1ms gap, gave #{f32_gap}"
      assert_equal 1000, LinkAPI.beats_to_microbeats(b) - LinkAPI.beats_to_microbeats(a)
    end

    def test_negative_and_zero_beats
      assert_equal 0, LinkAPI.beats_to_microbeats(0.0)
      assert_equal(-1_500_000, LinkAPI.beats_to_microbeats(-1.5))
      assert_in_delta(-1.5, LinkAPI.microbeats_to_beats(-1_500_000), 1e-9)
    end

    # The wire contract: the beat argument must be encoded with the int64 type
    # tag ('h'), not float32 ('f'). Pins the encoding itself, so a future change
    # back to a bare Float fails here rather than silently costing 2ms.
    def test_beat_is_encoded_as_int64_on_the_wire
      encoder = ::SonicPi::OSC::OscEncode.new
      msg = encoder.encode_single_message(
        "/clockwork/clock/rpc/time_at_beat",
        [::SonicPi::OSC::Int64.new(LinkAPI.beats_to_microbeats(LARGE_BEAT)), 4.0, 1])
      # Type tag string follows the padded address; ",hfi" = int64, float32, int32.
      assert_includes msg, ",hfi", "expected an int64 beat, float32 quantum, int32 token"

      decoded = ::SonicPi::OSC::OscDecode.new.decode_single_message(msg)
      assert_equal "/clockwork/clock/rpc/time_at_beat", decoded[0]
      assert_equal 40_000_001_000, decoded[1][0]
      assert_in_delta LARGE_BEAT, LinkAPI.microbeats_to_beats(decoded[1][0]), 1e-6
    end
  end
end
