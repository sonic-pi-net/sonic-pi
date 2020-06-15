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

      args_to_test = [[1], [-1], [100], [-100], [1.0, 1.0], [0, 1], [0, 0, 0], [1, 0, 1, 1, 0, 1], [1, 0.0, 1.0, 0], [1.0, 1, 1], [-1, -1, -1], [1, 0, -1], ["eggs", "foo","bar", "beans", 0, -1, 2.0, -2000]]

      args_to_test.each do |args|
        m = encoder.encode_single_message(address, args)
        d_address, d_args = decoder.decode_single_message(m)
        assert_equal(address, d_address)
        assert_equal(args, d_args)
      end
    end
  end
end
