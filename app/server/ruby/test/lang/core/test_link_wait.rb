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

  # `link` must wait via link_sleep (which a session tempo-change broadcast
  # wakes early), not a fixed Kernel.sleep: a tempo change landing mid-wait
  # moves the target bar boundary, so the wait has to re-derive the (fixed)
  # target beat's wall time from the live timeline and adjust.
  class LinkWaitTester < Minitest::Test

    class TempoChangingLinkAPI
      attr_reader :sleeps

      def initialize
        @t0 = Time.now.to_f
        @tempo = 60.0
        @sleeps = []
      end

      def link_tempo(force = false, tl: "link"); @tempo; end
      def link_is_playing?(tl: "link"); true; end

      def link_get_clock_time_at_beat(beat, quantum = 4, tl: "link")
        @t0 + (beat * 60.0 / @tempo)
      end

      def link_get_beat_at_clock_time(t, quantum = 4, tl: "link")
        (t - @t0) * @tempo / 60.0
      end

      # `link` targets beat 4: 4s away at the initial 60 BPM.
      def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t)
        [4.0, link_get_clock_time_at_beat(4.0)]
      end

      # First wait: jump to 240 BPM (beat 4 is now ~1s after t0) and wake, as
      # the real link_sleep does when the tempo-change CV is broadcast. The
      # capped real sleep stands in for the CV timeout so the wait loop makes
      # wall-clock progress.
      def link_sleep(s)
        @sleeps << s
        @tempo = 240.0 if @sleeps.length == 1
        Kernel.sleep([s, 0.3].min)
        yield
      end
    end

    def setup
      @lang = SonicPi::MockLang.new
      @link = TempoChangingLinkAPI.new
      @lang.instance_variable_set(:@link_api, @link)
    end

    def test_link_wait_tracks_mid_wait_tempo_change
      state = {}
      t_start = Time.now.to_f
      @lang.run do
        link
        state[:beat]    = __get_spider_beat
        state[:elapsed] = Time.now.to_f - t_start
      end
      refute_nil state[:elapsed], "link block did not complete"
      # The anchored target beat is unchanged by the re-derive...
      assert_equal 4.0, state[:beat]
      # ...but its wall time tracked the 60 -> 240 BPM jump. The fixed
      # Kernel.sleep waits the full ~3.8s computed at 60 BPM.
      assert_operator state[:elapsed], :<, 2.0
      assert_operator @link.sleeps.length, :>=, 1, "wait did not go via link_sleep"
    end
  end
end
