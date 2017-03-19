#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/util"
require_relative "../../../lib/sonicpi/sample_loader"
require 'mocha/setup'

module SonicPi

  class SampleDurationTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new

      @sample_loader = Object.new
      @sample_loader.stubs(:find_candidates).returns("/foo.wav")
      @lang.sample_loader = @sample_loader


      @mock_samp = Object.new
      @mock_samp.stubs(:duration).returns(8)
      @mock_samp.stubs(:onset_slices).returns([{:start => 0, :finish => 0.125}, {:start => 0.125, :finish => 1}])
      @mock_samp.stubs(:slices).returns([{:start => 0, :finish => 0.5}, {:start => 0.5, :finish => 1}])

      @lang.mod_sound_studio.stubs(:load_sample).returns(@mock_samp)
    end

    def test_duration_of_samples
      @lang.run do
        assert_equal 8, sample_duration(:foo)
        assert_equal 16, sample_duration(:foo, rate: 0.5)
        assert_equal 16, sample_duration(:foo, rate: 0.5)
        assert_equal 4,  sample_duration(:foo, rate: 2)
        assert_equal 4,  sample_duration(:foo, rate: -2)
        assert_equal 8,  sample_duration(:foo, rate: 1, attack: 1)
        assert_equal 8,  sample_duration(:foo, rate: 1, release: 1)
        assert_equal 1,  sample_duration(:foo, rate: 1, sustain: 0, release: 1)
        assert_equal 2,  sample_duration(:foo, rate: 1, sustain: 1, release: 1)
        assert_equal 8,  sample_duration(:foo, rate: 1, sustain: -1, release: 12)
        assert_equal 4,  sample_duration(:foo, rate: 1, rpitch: 12)
        assert_equal 3,  sample_duration(:foo, rate: 1, beat_stretch: 3)
        assert_equal 1,  sample_duration(:foo, rate: 1, pitch_stretch: 1)
        assert_equal 4,  sample_duration(:foo, rate: 1, start: 0.5), 4
        assert_equal 2,  sample_duration(:foo, rate: 1, start: 0.5, finish: 0.75)
        assert_equal 2,  sample_duration(:foo, rate: 1, finish: 0.5, start: 0.75)
        assert_equal 1,  sample_duration(:foo, rate: 2, finish: 0.5, start: 0.75)
        assert_equal 1,  sample_duration(:foo, rate: 1, onset: 0)
        assert_equal 7,  sample_duration(:foo, rate: 1, onset: 1)
        assert_equal 4,  sample_duration(:foo, rate: 1, slice: 1)
      end

    end

  end
end
