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

require_relative "./setup_test"
require_relative "../lib/sonicpi/note"
require_relative "../lib/sonicpi/tuning"


module SonicPi
  class TuningTester < Minitest::Test
    # Testing with A 440Hz
    #  440Hz == :a4
    def test_just
      # 550Hz == :cs5 in :just tuning
      assert_equal(Tuning.new.hz_to_midi(550.0), Tuning.new.resolve_tuning(:cs5, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(550.0), Tuning.new.resolve_tuning(73, :just, :a))
      assert_in_delta(Tuning.new.hz_to_midi(550.0), Tuning.new.resolve_tuning(73.0, :just, :a), 0.00000001)

      # 660Hz == :e5 in :just tuning
      assert_equal(Tuning.new.hz_to_midi(660.0), Tuning.new.resolve_tuning(:e5, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(660.0), Tuning.new.resolve_tuning(76, :just, :a))
      assert_in_delta(Tuning.new.hz_to_midi(660.0), Tuning.new.resolve_tuning(76.0, :just, :a), 0.00000001)

      # 618.75Hz == :eb5 in :just tuning
      assert_equal(Tuning.new.hz_to_midi(618.75), Tuning.new.resolve_tuning(:eb5, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(618.75), Tuning.new.resolve_tuning(75, :just, :a))
      assert_in_delta(Tuning.new.hz_to_midi(618.75), Tuning.new.resolve_tuning(75.0, :just, :a), 0.00000001)
    end

    # test floating point resolutions between values
    def test_quarter_tones
      # 550Hz == :cs5 in :just tuning
      # 586.66666666667Hz == :d5 in :just tuning
      assert_in_delta(Tuning.new.hz_to_midi(568.33333333), Tuning.new.resolve_tuning(73.5, :just, :a), 0.00000001)
    end

    # test across multiple octaves
    def test_just_octaves
      assert_equal(Tuning.new.hz_to_midi(17.1875), Tuning.new.resolve_tuning(:cs0, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(34.375), Tuning.new.resolve_tuning(:cs1, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(68.75), Tuning.new.resolve_tuning(:cs2, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(137.5), Tuning.new.resolve_tuning(:cs3, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(275.0), Tuning.new.resolve_tuning(:cs4, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(550.0), Tuning.new.resolve_tuning(:cs5, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(1100.0), Tuning.new.resolve_tuning(:cs6, :just, :a))
      assert_equal(Tuning.new.hz_to_midi(2200.0), Tuning.new.resolve_tuning(:cs7, :just, :a))
    end

    # The following test that the tuning ratios relative to A 440Hz
    # should step up through each notes with the midi conversions staying
    # within a certain delta of accuracy (e.g. 0.00000001). This is basically
    # to prove that the midi conversion doesn't audibly detract from the ratios
    # defined for each tuning.
    def test_just_full_range
      range_tests_for_tuning(:just)
    end

    def test_pythagorean_full_range
      range_tests_for_tuning(:pythagorean)
    end

    def test_meantone_full_range
      range_tests_for_tuning(:meantone)
    end

    private

    def range_tests_for_tuning(tuning)
      # Tests accuracy from :a0 to :a6
      ratios = Tuning.new.tunings[tuning]
      ratios.each_with_index do |r, i|
        assert_in_delta(Tuning.new.hz_to_midi(27.5 * r.to_f), Tuning.new.resolve_tuning((21 + i), tuning, :a), 0.00000001)
        assert_in_delta(Tuning.new.hz_to_midi(55.0 * r.to_f), Tuning.new.resolve_tuning((33 + i), tuning, :a), 0.00000001)
        assert_in_delta(Tuning.new.hz_to_midi(110.0 * r.to_f), Tuning.new.resolve_tuning((45 + i), tuning, :a), 0.00000001)
        assert_in_delta(Tuning.new.hz_to_midi(220.0 * r.to_f), Tuning.new.resolve_tuning((57 + i), tuning, :a), 0.00000001)
        assert_in_delta(Tuning.new.hz_to_midi(440.0 * r.to_f), Tuning.new.resolve_tuning((69 + i), tuning, :a), 0.00000001)
        assert_in_delta(Tuning.new.hz_to_midi(880.0 * r.to_f), Tuning.new.resolve_tuning((81 + i), tuning, :a), 0.00000001)
      end
    end

  end
end
