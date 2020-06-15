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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/util"
require_relative "../../../lib/sonicpi/chord"


module SonicPi
  module Lang
    module WesternTheory
      module_function :chord_invert
      module_function :chord
    end
  end
  class InvertChordTester < Minitest::Test

    def test_inversion_of_basic_major
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), 0), [60, 64, 67].ring)
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), 1), [64, 67, 72].ring)
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), 2), [67, 72, 76].ring)

      # what should happen the other way...
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), -1).sort, [55, 60, 64].ring)
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), -2).sort, [52, 55, 60].ring)
      assert_equal(Lang::WesternTheory.chord_invert(Chord.new(:C4, :major), -3).sort, [48, 52, 55].ring)

      # edge case
      assert_equal(Lang::WesternTheory.chord_invert(Lang::WesternTheory.chord(:C4, "1"), 1), [72].ring)
      assert_equal(Lang::WesternTheory.chord_invert(Lang::WesternTheory.chord(:C4, "1"), -1), [48].ring)
    end

  end
end
