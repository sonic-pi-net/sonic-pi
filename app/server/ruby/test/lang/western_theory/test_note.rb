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
require_relative "../../../lib/sonicpi/lang/core"

require 'mocha/setup'

module SonicPi
  class NoteTester < Minitest::Test

    def setup
      @mock_sound = Object.new
      @mock_sound.extend(Lang::WesternTheory)
      @mock_sound.extend(Lang::Core)
    end

    def test_sample_with_various_args
      Note.expects(:resolve_midi_note_without_octave).with(:c4)
      @mock_sound.send(:note, :c4)

      Note.expects(:resolve_midi_note).with(:c, 5)
      @mock_sound.send(:note, :c, octave: 5)
    end

  end
end
