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

  # Spider-side coverage for `use_bpm :midi` / `use_bpm :midi, "port"`. The
  # engine timeline math is covered by SuperSonic's native tests; here we pin the
  # Ruby thin-consumer plumbing against a link double that returns a programmable
  # tempo per timeline name.
  #
  # NOTE: MockLang#run instance_evals the block in a sub-thread, so Minitest
  # assertions can't run inside it — we capture results into closed-over locals
  # and assert *after* run returns.
  class BpmMidiTester < Minitest::Test

    class TimelineLinkAPI
      attr_reader :tempos
      def initialize; @tempos = { "link" => 60.0 }; end
      def tempo_for(tl); @tempos[tl] || 60.0; end

      def link_tempo(force = false, tl: "link"); tempo_for(tl); end
      def link_is_playing?(tl: "link"); false; end
      def link_set_bpm!(*); end
      def link_sleep(*); end
      # Simulate the engine's beat<->clock-time math (proportional to tempo).
      def link_get_beat_at_clock_time(clock_time, quantum = 4, tl: "link")
        clock_time * tempo_for(tl) / 60.0
      end
      def link_get_clock_time_at_beat(beat, quantum = 4, tl: "link")
        beat * 60.0 / tempo_for(tl)
      end
      def link_get_next_beat_and_clock_time_at_phase(*); [0.0, 0.0]; end

      def clock_timelines
        [{ name: "link",       raw: "link",   bpm: 60.0,  clocking: true,  stale: false, primary: false },
         { name: "midi:portA", raw: "Port A", bpm: 140.0, clocking: true,  stale: false, primary: true  },
         { name: "midi:portB", raw: "Port B", bpm: 90.0,  clocking: false, stale: true,  primary: false }]
      end
    end

    def setup
      @lang = SonicPi::MockLang.new
      @link = TimelineLinkAPI.new
      @lang.instance_variable_set(:@link_api, @link)
    end

    def test_mode_round_trips
      modes = {}
      @lang.run do
        use_bpm :midi;          modes[:midi_nil]  = current_bpm_mode
        use_bpm :midi, "portA"; modes[:midi_port] = current_bpm_mode
        use_bpm :link;          modes[:link]      = current_bpm_mode
        use_bpm 90;             modes[:num]       = current_bpm_mode
      end
      assert_equal([:midi, nil, 4.0],     modes[:midi_nil])
      assert_equal([:midi, "portA", 4.0], modes[:midi_port])
      assert_equal(:link,            modes[:link])
      assert_equal(90.0,             modes[:num])
    end

    def test_quantum_opt_round_trips
      modes = {}
      @lang.run do
        use_bpm :midi, "portA", quantum: 8; modes[:port_q] = current_bpm_mode
        use_bpm :midi, quantum: 1;          modes[:bare_q] = current_bpm_mode
      end
      assert_equal([:midi, "portA", 8.0], modes[:port_q])
      assert_equal([:midi, nil, 1.0],     modes[:bare_q])
    end

    def test_quantum_rejected_outside_midi_mode
      results = {}
      probes = { link: [:link, nil, { quantum: 4 }], num: [90, nil, { quantum: 4 }], zero: [:midi, nil, { quantum: 0 }] }
      @lang.run do
        probes.each do |key, args|
          begin
            use_bpm(*args)
            results[key] = :no_raise
          rescue ArgumentError
            results[key] = :raised
          end
        end
      end
      probes.each_key { |k| assert_equal(:raised, results[k], "use_bpm #{probes[k].inspect} should raise ArgumentError") }
    end

    def test_current_bpm_follows_named_midi_timeline
      @link.tempos["midi:portA"] = 140.0
      got = nil
      @lang.run do
        use_bpm :midi, "portA"
        got = current_bpm
      end
      assert_equal(140.0, got)
    end

    def test_bare_midi_follows_engine_primary
      @link.tempos["midi"] = 128.0   # engine resolves bare "midi" to its primary
      got = nil
      @lang.run do
        use_bpm :midi
        got = current_bpm
      end
      assert_equal(128.0, got)
    end

    def test_validation_rejects_bad_args
      results = {}
      probes = { empty: [:midi, ""], non_string: [:midi, 5], bogus: [:bogus], negative: [-5] }
      @lang.run do
        probes.each do |key, args|
          begin
            use_bpm(*args)
            results[key] = :no_raise
          rescue ArgumentError
            results[key] = :raised
          end
        end
      end
      probes.each_key { |k| assert_equal(:raised, results[k], "use_bpm #{probes[k].inspect} should raise ArgumentError") }
    end

    def test_with_bpm_midi_restores_prior_mode
      inside = nil
      after  = nil
      @lang.run do
        use_bpm 90
        with_bpm :midi, "portA" do
          inside = current_bpm_mode
        end
        after = current_bpm_mode
      end
      assert_equal([:midi, "portA", 4.0], inside)
      assert_equal(90.0, after)
    end

    def test_midi_clock_sources_lists_only_midi_rows
      srcs = nil
      @lang.run { srcs = midi_clock_sources }
      assert_equal(2, srcs.length)              # the link row is excluded
      a = srcs.find { |s| s[:port] == "portA" }
      refute_nil(a)
      assert_equal("Port A", a[:name])
      assert_equal(140.0, a[:bpm])
      assert(a[:primary])
      b = srcs.find { |s| s[:port] == "portB" }
      assert(b[:stale])
      refute(b[:clocking])
    end
  end
end
