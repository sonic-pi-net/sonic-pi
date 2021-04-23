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

require_relative './setup_test'
require_relative '../lib/sonicpi/samplebuffer'
require 'mocha/setup'

module SonicPi
  class SampleBufferTester < Minitest::Test
    def setup
      @mock_buffer = SampleBuffer.new(nil, '/foo.wav')
    end

    def test_onset_slices_with_no_onsets
      @mock_buffer.stubs(:onsets).returns([])
      expected_slices = [{ start: 0, finish: 1, index: 0 }].ring

      assert_equal(@mock_buffer.onset_slices, expected_slices)
    end

    def test_onset_slices_with_single_onset
      @mock_buffer.stubs(:onsets).returns([0])
      expected_slices = [{ start: 0, finish: 1, index: 0 }].ring

      assert_equal(@mock_buffer.onset_slices, expected_slices)
    end

    def test_onset_slices_with_multiple_onsets
      @mock_buffer.stubs(:onsets).returns([0, 0.5])
      expected_slices = [
        { start: 0, finish: 0.5, index: 0 },
        { start: 0.5, finish: 1, index: 1 }
      ].ring

      assert_equal(@mock_buffer.onset_slices, expected_slices)
    end
  end
end
