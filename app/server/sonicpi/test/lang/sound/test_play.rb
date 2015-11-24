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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/atom"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"
require 'mocha/setup'
require 'ostruct'

module SonicPi
  class PlayTester < Minitest::Test
    def setup
      @mock_sound = Object.new
      @mock_sound.extend(Lang::Sound)
      @mock_sound.extend(Lang::Core)
      @mock_sound.stubs(:sleep) # avoid loading Spider class
      @mock_sound.stubs(:ensure_good_timing!) # avoid loading Spider class
      @mock_sound.stubs(:__delayed_user_message)
      @mock_sound.stubs(:current_synth_name).returns(:beep)
    end

    def test_play_with_various_args
      @mock_sound.expects(:trigger_inst).with(:beep, {note: 60.0})
      @mock_sound.play :c

      @mock_sound.expects(:trigger_inst).with(:beep, {note: 60.0, release: 0.1})
      @mock_sound.play :c, release: 0.1

      # Single hash
      @mock_sound.expects(:trigger_inst).with(:beep, {note: :c, release: 0.1})
      @mock_sound.play({note: :c, release: 0.1})

      # Hash and args
      @mock_sound.expects(:trigger_inst).with(:beep, {note: :c, amp: 1, release: 0.1})
      @mock_sound.play({note: :c, amp: 1}, {release: 0.1})
    end

  end
end
