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
require_relative "../lib/sonicpi/note"
require_relative "../lib/sonicpi/scale"
require_relative "../lib/sonicpi/chord"

module SonicPi
  class ChordTester < Minitest::Test

    def test_resolution_of_basic_major
      assert_equal(Chord.new(:C4, :major), [60, 64, 67])
      assert_equal(Chord.new(60, :major), [60, 64, 67])
    end

    def test_resolution_of_chord_degrees
      assert_equal(Chord.resolve_degree(:i,   :C4, :major, 3),  [60, 64, 67])
      assert_equal(Chord.resolve_degree(:ii,  :C4, :major, 3),  [62, 65, 69])
      assert_equal(Chord.resolve_degree(5,    :D4, :major, 4),  [69, 73, 76,79])

      assert_equal(Chord.resolve_degree(:vii, :D4, :major, 4),  [73, 76,79, 83])
      assert_equal(Chord.resolve_degree(:vii, :F4, :ionian, 4), [76, 79, 82, 86])
    end

    def test_resolution_of_chord_degrees_with_many_notes
      assert_equal(Chord.resolve_degree(:vii, :C4, :major, 6),  [71, 74, 77, 81, 84, 88])
      assert_equal(Chord.resolve_degree(:vii, :C4, :major, 7),  [71, 74, 77, 81, 84, 88, 91])
    end

    def test_resolution_of_ninth_sharp_five
      assert_equal(Chord.new(:C4, "9+5"),  [60, 64, 68, 70, 74])
      assert_equal(Chord.new(:C4, "m9+5"), [60, 63, 68, 70, 74])
    end

    def test_resolution_of_ninth_flat_five
      assert_equal(Chord.new(:C4, "9-5"),  [60, 64, 66, 70, 74])
    end

    def test_resolution_of_minor_major_seven
      assert_equal(Chord.new(:C4, :minor_major7), [60, 63, 67, 71])
      assert_equal(Chord.new(:C4, "mM7"),         [60, 63, 67, 71])
      assert_equal(Chord.new(:C4, "mmaj7"),       [60, 63, 67, 71])
    end

    def test_resolution_of_maj13
      assert_equal(Chord.new(:C4, :maj13), [60, 64, 67, 71, 74, 77, 81])
    end

    def test_seven_sharp_nine_is_alias_of_seven_flat_ten
      assert_equal(Chord.new(:C4, "7-10"), Chord.new(:C4, "7+9"))
    end

    def test_maj7_and_min7_aliases
      assert_equal(Chord.new(:C4, :major7), Chord.new(:C4, :maj7))
      assert_equal(Chord.new(:C4, :minor7), Chord.new(:C4, :min7))
    end

  end
end
