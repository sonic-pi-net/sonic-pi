#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/mods/sound"

module SonicPi
  module Mods
    module Sound
      module_function :sample_duration
      module_function :pitch_to_ratio

      # mock out load_sample to always return a sample with duration 8
      def self.load_sample(path)
        mock_samp = Struct.new(:duration)
        mock_samp.new(8)
      end

      # mock out current bpm to be 60
      def self.current_bpm
        60
      end
    end
  end

  class SampleDurationTester < Test::Unit::TestCase

    def test_duration_of_samples
      assert_equal(8,  Mods::Sound.sample_duration(:foo))
      assert_equal(16, Mods::Sound.sample_duration(:foo, rate: 0.5))
      assert_equal(4,  Mods::Sound.sample_duration(:foo, rate: 2))
      assert_equal(4,  Mods::Sound.sample_duration(:foo, rate: -2))
      assert_equal(8,  Mods::Sound.sample_duration(:foo, rate: 1, attack: 1))
      assert_equal(8,  Mods::Sound.sample_duration(:foo, rate: 1, release: 1))
      assert_equal(1,  Mods::Sound.sample_duration(:foo, rate: 1, sustain: 0, release: 1))
      assert_equal(4,  Mods::Sound.sample_duration(:foo, rate: 1, rpitch: 12))
      assert_equal(3,  Mods::Sound.sample_duration(:foo, rate: 1, beat_stretch: 3))
      assert_equal(1,  Mods::Sound.sample_duration(:foo, rate: 1, pitch_stretch: 1))
      assert_equal(4,  Mods::Sound.sample_duration(:foo, rate: 1, start: 0.5), 4)
      assert_equal(2,  Mods::Sound.sample_duration(:foo, rate: 1, start: 0.5, finish: 0.75))
      assert_equal(2,  Mods::Sound.sample_duration(:foo, rate: 1, finish: 0.5, start: 0.75))
      assert_equal(1,  Mods::Sound.sample_duration(:foo, rate: 2, finish: 0.5, start: 0.75))
    end

  end
end
