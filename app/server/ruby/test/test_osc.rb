#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "./setup_test"
require_relative "../lib/sonicpi/osc/osc"

module SonicPi

  class OSCTester < Minitest::Test

    # OSC's own type for a time. clockwork puts one on the end of every MIDI
    # and gamepad event: the moment it arrived. A decoder that refused it
    # dropped the whole event.
    def test_a_timetag_argument_decodes_as_a_timetag
      decoder = ::SonicPi::OSC::OscDecode.new(true)
      tt = 0xE000_0000_8000_0001
      m = "/x\0\0,it\0".b + [7].pack("N") + [tt].pack("Q>")
      address, args = decoder.decode_single_message(m)
      assert_equal("/x", address)
      assert_equal(7, args[0])
      assert_kind_of(::SonicPi::OSC::TimeTag, args[1])
      assert_equal(tt, args[1].to_i)
    end

    def test_a_timetag_round_trips
      decoder = ::SonicPi::OSC::OscDecode.new(true)
      encoder = ::SonicPi::OSC::OscEncode.new(true)
      tt = 0xE123_4567_89AB_CDEF
      m = encoder.encode_single_message("/t", [1, ::SonicPi::OSC::TimeTag.new(tt)])
      _, args = decoder.decode_single_message(m)
      assert_equal(1, args[0])
      assert_equal(tt, args[1].to_i)
    end

    def test_basic_address_encoding
      decoder = ::SonicPi::OSC::OscDecode.new(true)
      encoder = ::SonicPi::OSC::OscEncode.new(true)

      address = "/foo"

      m = encoder.encode_single_message(address)
      d_address, d_args = decoder.decode_single_message(m)
      assert_equal(address, d_address)
      assert_equal([], d_args)
    end


    def test_args_encoding_multiple
      decoder = ::SonicPi::OSC::OscDecode.new(true)
      encoder = ::SonicPi::OSC::OscEncode.new(true)

      address = "/feooblah"

      args_to_test = [
        [1],
        [-1],
        [100],
        [-100],
        [1.0, 1.0],
        [0, 1],
        [0, 0, 0],
        [1, 0, 1, 1, 0, 1],
        [1, 0.0, 1.0, 0],
        [1.0, 1, 1],
        [-1, -1, -1],
        [1, 0, -1],
        [true],
        [false],
        ["eggs", "foo","bar", "beans", 0, -1, 2.0, -2000, true, false]
      ]

      args_to_test.each do |args|
        m = encoder.encode_single_message(address, args)
        d_address, d_args = decoder.decode_single_message(m)
        assert_equal(address, d_address)
        assert_equal(args, d_args)
      end
    end
  end
end
