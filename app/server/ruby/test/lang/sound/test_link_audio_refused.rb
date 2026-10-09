#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
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
require_relative "../../../lib/sonicpi/runtime"

module SonicPi
  # What link_audio says when the engine will not open a stream. The engine
  # refuses a channel no peer publishes, and one it has no room left for, and
  # says only that it refused: the language works out which, and what to do.
  # It used to say nothing at all, and play silence.
  class LinkAudioRefusedTester < Minitest::Test
    class MockStudio
      def initialize(mock_sound_studio, _mock_loader, _mock_tmp_path)
        @mod_sound_studio = mock_sound_studio
      end
    end

    def setup
      @studio = mock
      @studio.stubs(:cent_tuning).returns(0)
      @studio.stubs(:ensure_link_audio_input).returns(nil)
      @link = mock
      @sound = MockStudio.new(@studio, nil, nil)
      @sound.extend(Lang::Sound)
      @sound.extend(Lang::Core)
      @sound.extend(RuntimeMethods)
      @sound.instance_variable_set(:@link_api, @link)
      @sound.stubs(:__delayed_message)
      @sound.send(:__set_default_user_thread_locals!)
    end

    def test_a_peer_nobody_publishes_names_the_ones_that_are_there
      @link.stubs(:link_audio_channels).returns([["Ableton Live", "Main"], ["Sonic Pi", "Main"]])
      e = assert_raises(RuntimeError) { @sound.link_audio "Sonic Piss" }
      assert_includes e.message, '"Sonic Piss"'
      assert_includes e.message, '"Ableton Live", "Main"'
      assert_includes e.message, '"Sonic Pi", "Main"'
    end

    def test_with_nothing_published_it_says_so
      @link.stubs(:link_audio_channels).returns([])
      e = assert_raises(RuntimeError) { @sound.link_audio "Live" }
      assert_includes e.message, "no Link peer is publishing audio"
    end

    def test_a_channel_that_is_published_but_refused_means_every_input_is_in_use
      @link.stubs(:link_audio_channels).returns([["Live", "Main"]])
      e = assert_raises(RuntimeError) { @sound.link_audio "Live" }
      assert_includes e.message, "every Link Audio input is in use"
    end
  end
end
