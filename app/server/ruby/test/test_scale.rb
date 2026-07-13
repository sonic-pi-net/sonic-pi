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
require_relative "../lib/sonicpi/scale"
require_relative "../lib/sonicpi/note"

module SonicPi
  class NoteTester < Minitest::Test

    def test_tonic_capitalisation_should_make_no_difference
      assert_equal(Scale.new(:fs,:major,2), Scale.new(:Fs, :major, 2))
    end

    def test_tonic_capitalisation_should_make_no_difference_cont
      assert_equal(Scale.new(:fs,:major,2), Scale.new(:fS, :major, 2))
    end

    def test_tonic_capitalisation_should_make_no_difference_with_octave
      assert_equal(Scale.new(:Eb3,:major,2), Scale.new(:eb3, :major, 2), Scale.new(:EB3, :major, 2))
    end

    def test_tonic_default_octave_is_4
      assert_equal(Scale.new(:fs4,:major,2), Scale.new(:Fs, :major, 2))
    end


    def test_resolution_of_degree
      assert_equal(57, Scale.resolve_degree(:i, :A3, :minor))
      assert_equal(60, Scale.resolve_degree(:iii, :A3, :minor))
      assert_equal(61, Scale.resolve_degree(:iii, :A3, :major))
      assert_equal(60, Scale.resolve_degree(:diii, :A3, :major))
      assert_equal(61, Scale.resolve_degree(:Aiii, :A3, :minor))

      assert_equal(57, Scale.resolve_degree(1, :A3, :minor))
      assert_equal(60, Scale.resolve_degree(3, :A3, :minor))
      assert_equal(61, Scale.resolve_degree(3, :A3, :major))
      assert_equal(60, Scale.resolve_degree('d3', :A3, :major))
      assert_equal(61, Scale.resolve_degree('A3', :A3, :minor))

      assert_equal(69, Scale.resolve_degree(:viii, :A3, :minor))
      assert_equal(81, Scale.resolve_degree(:xv, :A3, :minor))
    end

    def test_resolution_of_degree_on_non_octave_scales
      two_octaves = Scale.new(:c4, :evic, 2).notes
      (1..7).each do |d|
        assert_in_delta(two_octaves[d - 1], Scale.resolve_degree(d, :c4, :evic), 0.0001)
      end
    end

    def test_lydian_dominant
      assert_equal([60, 62, 64, 66, 67, 69, 70, 72], Scale.new(:C4, :lydian_dominant).notes)
      assert_equal(Scale.new(:C4, :lydian_dominant), Scale.new(:C4, :acoustic))
    end

    def test_scale_aliases
      assert_equal(Scale.new(:C4, :super_locrian), Scale.new(:C4, :altered))
      assert_equal(Scale.new(:C4, :spanish), Scale.new(:C4, :phrygian_dominant))
      assert_equal(Scale.new(:C4, :bhairav), Scale.new(:C4, :double_harmonic))
      assert_equal(Scale.new(:C4, :bhairav), Scale.new(:C4, :byzantine))
    end

    def test_degree_invalid
      assert_raises Scale::InvalidDegreeError do
        Scale.resolve_degree(:joe, :A3, :major)
      end
    end

    def test_too_low_degree
      assert_raises Scale::InvalidDegreeError do
        Scale.resolve_degree(-1, :A3, :major)
      end
    end

    def test_degree_invalid_scale
      assert_raises Scale::InvalidScaleError do
        Scale.resolve_degree(:i, :A3, :wooble)
      end
    end

    def test_degree_invalid_tonic
      assert_raises Note::InvalidNoteError do
        Scale.resolve_degree(:i, :blah, :minor)
      end
    end

  end
end
