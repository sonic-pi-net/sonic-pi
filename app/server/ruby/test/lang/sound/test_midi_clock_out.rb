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
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi

  # Spider-side coverage for `midi_clock_out`: the engine generates the ticks;
  # here we pin that the thread pushes the right tempo SOURCE for its bpm-mode
  # (fixed number vs :link / :midi timeline) and re-pushes on a use_bpm change.
  # The recording double stands in for the engine-bound MidiAPI.
  class MidiClockOutTester < Minitest::Test

    class RecordingMidiAPI
      attr_reader :calls
      def initialize; @calls = []; end
      def midi_clock_out_bpm(port, bpm);    @calls << [:bpm, port, bpm.to_f]; end
      def midi_clock_out_follow(port, tl);  @calls << [:follow, port, tl];    end
      def midi_clock_out_off(port);         @calls << [:off, port];           end
    end

    class TimelineLinkAPI
      attr_reader :tempos
      def initialize; @tempos = { "link" => 60.0 }; end
      def tempo_for(tl); @tempos[tl] || 60.0; end
      def link_tempo(force = false, tl: "link"); tempo_for(tl); end
      def link_is_playing?(tl: "link"); false; end
      def link_get_beat_at_clock_time(clock_time, quantum = 4, tl: "link"); clock_time * tempo_for(tl) / 60.0; end
      def link_get_clock_time_at_beat(beat, quantum = 4, tl: "link"); beat * 60.0 / tempo_for(tl); end
      def link_get_next_beat_and_clock_time_at_phase(*); [0.0, 0.0]; end
    end

    def setup
      @lang = SonicPi::MockLang.new
      @link = TimelineLinkAPI.new
      @midi = RecordingMidiAPI.new
      @lang.instance_variable_set(:@link_api, @link)
      @lang.instance_variable_set(:@midi_api, @midi)
    end

    def test_fixed_bpm_is_pushed_and_follows_use_bpm
      @lang.run do
        use_bpm 120
        midi_clock_out "moog"
        use_bpm 140                # bound thread → engine gets the new tempo
      end
      assert_includes @midi.calls, [:bpm, "moog", 120.0]
      assert_includes @midi.calls, [:bpm, "moog", 140.0]
    end

    def test_link_mode_follows_the_link_timeline
      @lang.run do
        use_bpm :link
        midi_clock_out "drums"
      end
      assert_includes @midi.calls, [:follow, "drums", "link"]
    end

    def test_midi_mode_follows_the_named_timeline
      @lang.run do
        use_bpm :midi, "portA"
        midi_clock_out "out"
      end
      assert_includes @midi.calls, [:follow, "out", "midi:portA"]
    end

    def test_off_unbinds_so_later_use_bpm_does_not_repush
      @lang.run do
        use_bpm 120
        midi_clock_out "moog"
        midi_clock_out "moog", off: true
        use_bpm 200
      end
      assert_includes @midi.calls, [:off, "moog"]
      refute_includes @midi.calls, [:bpm, "moog", 200.0]
    end

    def test_no_clock_out_bound_means_no_pushes
      @lang.run do
        use_bpm 120
        use_bpm 140
      end
      assert_empty @midi.calls
    end

    def test_density_does_not_scale_pushed_clock_tempo
      @lang.run do
        use_bpm 120
        midi_clock_out "moog"
        density 2 do
          use_bpm 120   # re-push inside density: raw bpm, not 240
        end
      end
      refute_includes @midi.calls, [:bpm, "moog", 240.0]
      assert_includes @midi.calls, [:bpm, "moog", 120.0]
    end

    def test_symbol_port_off_unbinds
      @lang.run do
        use_bpm 120
        midi_clock_out port: :moog
        midi_clock_out port: :moog, off: true
        use_bpm 200
      end
      assert_includes @midi.calls, [:off, "moog"]
      refute_includes @midi.calls, [:bpm, "moog", 200.0]
    end

    def test_binding_not_inherited_by_child_threads
      child_done = false
      @lang.run do
        use_bpm 120
        midi_clock_out "moog"
        in_thread do
          use_bpm 200            # child must NOT re-push to the parent's port
          child_done = true
        end
      end
      t = Time.now
      sleep 0.01 until child_done || Time.now - t > 5
      assert child_done, "child thread never ran"
      refute_includes @midi.calls, [:bpm, "moog", 200.0]
      assert_includes @midi.calls, [:bpm, "moog", 120.0]
    end
  end
end
