#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'test/unit'
require_relative "../lib/sonicpi/scale"

module SonicPi
  class NoteTester < Test::Unit::TestCase

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
  end
end
