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

  # midi_sync: wait until a MIDI clock timeline is anchored (a START/SPP has
  # defined its beat origin) and its transport is running, then sleep to the
  # next quantum boundary and follow the timeline. The engine's transport
  # state is ground truth; /midi:<port>/start etc. cues are only wake signals.
  class MidiSyncTester < Minitest::Test

    class MidiTimelineLinkAPI
      attr_accessor :playing, :anchored
      attr_reader :transport_polls, :sleeps

      def initialize
        @t0 = Time.now.to_f
        @tempo = 120.0
        @playing = false
        @anchored = false
        @transport_polls = 0
        @sleeps = []
      end

      def link_tempo(force = false, tl: "link"); @tempo; end
      def link_is_playing?(tl: "link"); @playing; end

      def link_transport_state(tl: "link")
        @transport_polls += 1
        { playing: @playing, anchored: @anchored }
      end

      def link_get_clock_time_at_beat(beat, quantum = 4, tl: "link")
        @t0 + (beat * 60.0 / @tempo)
      end

      def link_get_beat_at_clock_time(t, quantum = 4, tl: "link")
        (t - @t0) * @tempo / 60.0
      end

      def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t, tl: "link")
        now_beat = link_get_beat_at_clock_time(Time.now.to_f)
        next_beat = ((now_beat / quantum).floor + 1) * quantum + phase
        [next_beat, link_get_clock_time_at_beat(next_beat)]
      end

      def link_sleep(s)
        @sleeps << s
        Kernel.sleep([s, 0.3].min)
        yield
      end
    end

    def setup
      @lang = SonicPi::MockLang.new
      @link = MidiTimelineLinkAPI.new
      @lang.instance_variable_set(:@link_api, @link)
    end

    def test_anchored_and_playing_joins_next_bar_without_waiting_for_a_cue
      @link.playing = true
      @link.anchored = true
      state = {}
      @lang.run do
        midi_sync 4, port: "portA"
        state[:mode] = current_bpm_mode
        state[:beat] = __get_spider_beat
      end
      assert_equal([:midi, "portA", 4.0], state[:mode])
      refute_nil state[:beat]
      assert_equal(0.0, state[:beat] % 4.0)        # on a quantum boundary
      assert_operator state[:beat], :>, 0.0
    end

    def test_phase_arg_offsets_into_the_bar
      @link.playing = true
      @link.anchored = true
      state = {}
      @lang.run do
        midi_sync 4, 1, port: "portA"
        state[:beat] = __get_spider_beat
      end
      assert_equal(1.0, state[:beat] % 4.0)
    end

    def test_unanchored_timeline_waits_on_transport_cues
      # Not anchored: midi_sync must poll-after-cue, not proceed. Stub the cue
      # wait (covered by test_cue_sync) and flip the engine state on the wake.
      @link.playing = true
      @link.anchored = false
      cues = []
      @lang.stubs(:sync).with do |*args|
        cues << args
        @link.anchored = true                      # the START arrives
        true
      end.returns(nil)
      state = {}
      @lang.run do
        midi_sync 4, port: "portA"
        state[:mode] = current_bpm_mode
      end
      assert_equal(1, cues.length, "should sync exactly once for the cue")
      assert(cues[0].any? { |c| c.to_s == "/midi:portA*/start" },
             "should wait on the port's start cue, got: #{cues[0].inspect}")
      assert_operator @link.transport_polls, :>=, 2 # re-checked after the cue
      assert_equal([:midi, "portA", 4.0], state[:mode])
    end

    def test_no_port_waits_on_wildcard_cues
      @link.playing = false
      @link.anchored = false
      cues = []
      @lang.stubs(:sync).with do |*args|
        cues << args
        @link.playing = true
        @link.anchored = true
        true
      end.returns(nil)
      @lang.run { midi_sync }
      assert(cues[0].any? { |c| c.to_s == "/midi:*/start" },
             "bare midi_sync should wait on any port's start, got: #{cues[0].inspect}")
    end

    def test_rejects_bad_quantum
      result = nil
      @lang.run do
        begin
          midi_sync 0
          result = :no_raise
        rescue ArgumentError
          result = :raised
        end
      end
      assert_equal(:raised, result)
    end
  end
end
