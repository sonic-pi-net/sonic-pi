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
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"

module SonicPi
  module Lang
    module Sound
      module_function :note_range
      module_function :note
    end
  end
  class NoteRangeTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_note_range
      assert_equal(ring(*(60..72)), Lang::Sound.note_range(:c4,:c5))
      assert_equal(ring(60, 64, 67, 72), Lang::Sound.note_range(:c4,:c5, pitches: Chord.new(:c, :major)))
      assert_equal(ring(60, 62, 64, 65, 67, 69, 71, 72), Lang::Sound.note_range(:c4,:c5, pitches: Scale.new(:c, :major)))
      assert_equal(ring(67), Lang::Sound.note_range(:c4,:c5, pitches: [:g3]))

      # multiple octaves
      assert_equal(ring(60, 64, 67, 72, 76, 79, 84), Lang::Sound.note_range(:c4,:c6, pitches: Chord.new(:c, :major)))
    end

  end

end
