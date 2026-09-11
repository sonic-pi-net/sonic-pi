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
require_relative "../../../lib/sonicpi/atom"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"
require_relative "../../../lib/sonicpi/lang/midi"
require_relative "../../../lib/sonicpi/runtime"

module SonicPi
  # The track verbs: the track comes from use_track/with_track or track:,
  # and the MIDI ones reach the studio's server as the named track.
  class TracksTester < Minitest::Test
    class MockStudio
      def initialize(mock_sound_studio, _mock_loader, _mock_tmp_path)
        @mod_sound_studio = mock_sound_studio
      end
    end

    def setup
      @server = mock
      @studio = mock
      @studio.stubs(:cent_tuning).returns(0)
      @studio.stubs(:server).returns(@server)
      @studio.stubs(:track_lookup!).with { |n| %w[surge verb].include?(n.to_s) }.returns({})
      @studio.stubs(:track_lookup!).with { |n| !%w[surge verb].include?(n.to_s) }.raises("Unknown track")
      @studio.stubs(:track_touched)
      @sound = MockStudio.new(@studio, nil, nil)
      @sound.extend(Lang::Sound)
      @sound.extend(Lang::Core)
      @sound.extend(Lang::Midi)
      @sound.extend(RuntimeMethods)
      @sound.stubs(:sleep)
      @sound.stubs(:ensure_good_timing!)
      @sound.stubs(:__delayed_user_message)
      @sound.stubs(:__delayed_message)
      @sound.stubs(:time_warp)
      @sound.send(:init_tuning)
      @sound.send(:__set_default_user_thread_locals!)
      @sound.use_bpm 60
      # Thread-local, so one test's use_track would be the next test's current track.
      @sound.__thread_locals.set(:sonic_pi_mod_sound_current_track, nil)
    end

    def test_track_midi_needs_a_track
      e = assert_raises(RuntimeError) { @sound.track_midi :e3 }
      assert_match(/use_track :surge first, or pass track: :surge/, e.message)
    end

    def test_use_track_sets_the_current_track
      assert_nil @sound.current_track
      @sound.use_track :surge
      assert_equal :surge, @sound.current_track
      @server.expects(:track_note_on).with("surge", 52, 127, 1)
      @sound.track_midi :e3
    end

    def test_use_track_refuses_an_unknown_track
      assert_raises(RuntimeError) { @sound.use_track :nope }
      assert_nil @sound.current_track
    end

    def test_track_opt_names_the_track_and_is_not_a_plugin_param
      @server.expects(:track_note_on).with("verb", 52, 100, 2)
      @sound.track_midi :e3, 100, channel: 2, track: :verb
    end

    def test_with_track_is_for_the_block
      @sound.use_track :surge
      @server.expects(:track_note_on).with("verb", 52, 127, 1)
      @server.expects(:track_note_on).with("surge", 52, 127, 1)
      @sound.with_track :verb do
        @sound.track_midi_note_on :e3
      end
      assert_equal :surge, @sound.current_track
      @sound.track_midi_note_on :e3
    end

    def test_the_midi_family
      @sound.use_track :surge
      @server.expects(:track_note_off).with("surge", 52, 127, 1)
      @sound.track_midi_note_off :e3
      @server.expects(:track_cc).with("surge", 1, 64, 1)
      @sound.track_midi_cc 1, 64
      @server.expects(:track_pitch_bend).with("surge", 0.5, 1)
      @sound.track_midi_pitch_bend 0.75
      @server.expects(:track_all_notes_off).with("surge")
      @sound.track_midi_all_notes_off
    end

    def test_track_control_by_key_and_by_name
      @sound.use_track :surge
      @studio.expects(:track_param_lookup!).with("surge", :filter_1_cutoff).returns(["Filter 1 Cutoff", 7])
      @server.expects(:track_param).with("surge", "Filter 1 Cutoff", 0.5, nil, 7)
      @sound.track_control filter_1_cutoff: 0.5

      @studio.expects(:track_param_lookup!).with("verb", "Mix").returns(["Mix", 9])
      @server.expects(:track_param).with("verb", "Mix", 0.25, nil, 9)
      @sound.track_control "Mix", 0.25, track: :verb
    end

    # The lookup answers the range with the name and handle, and a value
    # outside it is refused rather than clamped: `line 3, 120` on a 0..1
    # parameter pinned Surge's cutoff open and looked like a sweep.
    def test_track_control_refuses_a_value_outside_the_plugins_range
      @sound.use_track :surge
      @studio.stubs(:track_param_lookup!).with { |t, k| t == "surge" && [:filter_1_cutoff, "A Filter 1 Cutoff"].include?(k) }.returns(["A Filter 1 Cutoff", 7, 0.0, 1.0])
      @server.expects(:track_param).with("surge", "A Filter 1 Cutoff", 1.0, nil, 7)
      @sound.track_control filter_1_cutoff: 1
      @server.expects(:track_param).never
      e = assert_raises(RuntimeError) { @sound.track_control filter_1_cutoff: 3 }
      assert_match(/"A Filter 1 Cutoff" on track :surge takes a value between 0.0 and 1.0, not 3/, e.message)
      e = assert_raises(RuntimeError) { @sound.track_control "A Filter 1 Cutoff", -0.5 }
      assert_match(/between 0.0 and 1.0, not -0.5/, e.message)
    end

    def test_a_note_carrying_a_parameter_is_checked_too
      @sound.use_track :surge
      @studio.stubs(:track_param_lookup!).with("surge", :filter_1_cutoff).returns(["A Filter 1 Cutoff", 7, 0.0, 1.0])
      @server.expects(:track_param).never
      @server.expects(:track_note_on).never
      assert_raises(RuntimeError) { @sound.track_midi :e3, filter_1_cutoff: 120 }
    end

    # with_send: mix: is with_fx's, 1 by default - wholly through the track.
    # sound_out_stereo puts pre_amp * in on its output and passes
    # amp * pre_amp * in on, so what is sent is pre_amp and what is kept is
    # amp * pre_amp.
    def test_with_send_mixes_like_with_fx
      @studio.stubs(:track_send_channel).with("verb").returns(3)
      @sound.expects(:with_fx).with(:sound_out_stereo, output: 3, pre_amp: 1.0, amp: 0.0)
      @sound.with_send(:verb) { }
      @sound.expects(:with_fx).with(:sound_out_stereo, output: 3, pre_amp: 0.25, amp: 2.0)
      @sound.with_send(:verb, amp: 0.5, mix: 0.5) { }
      # Nothing sent: the block runs as itself and no channel is asked for.
      @sound.expects(:with_fx).with(:level, amp: 1.0)
      @sound.with_send(:verb, mix: 0) { }
      @sound.expects(:with_fx).with(:level, amp: 0.5)
      @sound.with_send(:verb, amp: 0, mix: 0.5) { }
    end

    # Routing only: a plugin parameter is track_control's, and one given
    # here is refused with that verb named, not set on the way past.
    def test_with_send_takes_no_plugin_parameters
      @studio.stubs(:track_send_channel).with("verb").returns(3)
      @sound.expects(:with_fx).never
      @server.expects(:track_param).never
      e = assert_raises(RuntimeError) { @sound.with_send(:verb, decay_time: 4) { } }
      assert_match(/with_send takes amp:, mix: and track:, not :decay_time. .*track_control decay_time: 0.5/, e.message)
    end

    # Stop leaves a track a live_track had silent; playing or sending into
    # it is what brings it back (studio.rb, track_touched). Choosing one is
    # not: a run that does use_track and then live_track must never have
    # the track on the main mix in between.
    def test_playing_or_sending_into_a_track_touches_it_and_choosing_one_does_not
      @studio.stubs(:track_send_channel).with("verb").returns(3)
      @sound.stubs(:with_fx)
      @server.stubs(:track_note_on)
      @studio.expects(:track_touched).never
      @sound.use_track :surge
      @sound.with_track(:verb) { }
      @studio.expects(:track_touched).with("surge").once
      @sound.track_midi_note_on :e3
      @studio.expects(:track_touched).with("verb").once
      @sound.with_send(:verb) { }
    end

    # `live_track :name, :stop` hands the track back to the main mix; the
    # node's own death (Stop) only parks it, via the on_destroyed hook.
    def test_live_track_stop_releases_the_track
      @studio.stubs(:track_info).with("surge").returns({id: 7})
      @studio.expects(:kill_live_synth).with([:track, "surge"])
      @studio.expects(:track_monitor_release).with(7)
      @sound.live_track :surge, :stop
    end

    def test_track_control_needs_something_to_set
      @sound.use_track :surge
      e = assert_raises(RuntimeError) { @sound.track_control }
      assert_match(/track_control filter_1_cutoff: 0.5/, e.message)
    end
  end
end
