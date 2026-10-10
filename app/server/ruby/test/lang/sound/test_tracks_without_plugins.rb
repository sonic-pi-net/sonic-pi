#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"
require_relative "../../../lib/sonicpi/runtime"

module SonicPi
  # The track functions need plugin hosting. In a build without it each says
  # so, before it looks at anything it was given, rather than reaching for a
  # plugin host that is not there.
  class TracksWithoutPluginsTester < Minitest::Test
    TRACK_FUNCTIONS = %i[live_track with_send use_track with_track current_track
                         track_midi track_midi_note_on track_midi_note_off track_midi_cc
                         track_midi_pitch_bend track_midi_all_notes_off track_control tracks]

    def setup
      @before = ENV.delete("SONIC_PI_PLUGINS")
      @sound = Object.new
      @sound.extend(Lang::Sound)
    end

    def teardown
      ENV["SONIC_PI_PLUGINS"] = @before if @before
    end

    def test_every_track_function_says_it_needs_plugin_hosting
      TRACK_FUNCTIONS.each do |fn|
        arity = @sound.method(fn).arity
        given = Array.new(arity >= 0 ? arity : -arity - 1, :surge)
        e = assert_raises(RuntimeError, fn.to_s) { @sound.send(fn, *given) { } }
        assert_equal "#{fn} needs plugin hosting, which this build of Sonic Pi doesn't have.", e.message
      end
    end

    # The help hides what a build cannot do: the docs say which these are.
    def test_their_docs_say_they_need_plugins
      TRACK_FUNCTIONS.each do |fn|
        doc = Lang::Core.docs[fn]
        refute_nil doc, fn
        assert_equal :plugins, doc[:needs], fn
      end
      assert_nil Lang::Core.docs[:play][:needs]
    end
  end
end
