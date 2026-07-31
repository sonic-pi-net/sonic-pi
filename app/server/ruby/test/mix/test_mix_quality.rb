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

# Mix quality regression tests. Every assertion here is a measurement of a
# real render through the real mixer synthdef in a headless engine, so a
# change to the master chain that costs clarity or loudness fails here rather
# than being discovered on stage.
#
# These need the built engine and the harness synthdefs, so they skip rather
# than fail when either is missing.

require_relative "../setup_test"
require_relative "lib/mix_engine"
require_relative "lib/mix_signals"
require_relative "lib/mix_analysis"

module SonicPi
  class MixQualityTester < Minitest::Test
    A = MixAnalysis
    S = MixSignals

    @@engine = nil
    @@unavailable = nil

    def self.engine
      return @@engine if @@engine
      unless File.executable?(MixEngine::ENGINE)
        @@unavailable = "engine not built at #{MixEngine::ENGINE}"
        return nil
      end
      unless Dir.exist?(MixEngine::HARNESS_DEFS) && !Dir.empty?(MixEngine::HARNESS_DEFS)
        @@unavailable = "harness synthdefs not built (run synthdefs/build.sh)"
        return nil
      end
      @@engine = MixEngine.new.start
      Minitest.after_run { @@engine.stop }
      @@engine
    end

    def setup
      @engine = self.class.engine
      skip @@unavailable unless @engine
    end

    def sine
      @@sine ||= S.write(S.sine(freq: 1000.0, dbfs: -6.0, duration: 4.0), "test-sine1k")
    end

    def dnb
      @@dnb ||= S.write(S.dnb_burst(duration: 6.0), "test-dnb")
    end

    # Below the limiter's ceiling the mixer must not colour anything. The
    # floor here is float32, so this catches any stray gain, filter or
    # non-linearity introduced into the main path.
    def test_path_is_transparent_below_the_ceiling
      cap = @engine.render(sine, 4.0, {}).steady
      assert_in_delta(-6.0, A.db(A.sample_peak(cap.l)), 0.05, "unity gain")
      assert_operator A.thd_n_db(cap.l, 1000.0, cap.sample_rate), :<, -120.0, "THD+N"
      assert_operator A.db(A.dc_offset(cap.l).abs), :<, -100.0, "DC offset"
    end

    # The Drive dial reads as a percentage where 100 is unity, and the
    # percentage IS the gain, so the dial position divided by 100 must be
    # exactly the gain reaching the limiter. Values below 100 have to work
    # too: that is how headroom is made for a lot of layered sounds.
    #
    # This pins that one number fully describes the dial: the whole gain
    # lives in pre_amp, with no hidden scaling or fixed make-up stage
    # between the dial and the limiter.
    def test_drive_dial_gain_mapping
      # -20 dBFS in, so even full drive stays clear of the limiter and this
      # measures gain rather than gain-minus-limiting.
      quiet = S.write(S.sine(freq: 1000.0, dbfs: -20.0, duration: 4.0), "test-sine1k-quiet")
      [25, 50, 100, 200, 400].each do |pct|
        gain = pct / 100.0
        expected_db = 20.0 * Math.log10(gain)
        cap = @engine.render(quiet, 4.0, { pre_amp: gain }).steady
        assert_in_delta expected_db, A.db(A.sample_peak(cap.l)) + 20.0, 0.1,
                        "drive at #{pct}% on the dial"
      end
    end

    # Volume sits AFTER the limiter, so it can only scale the output. This is
    # the property the two-control split exists for: moving the fader must not
    # change the mix, only its level. Anything else means the fader is driving
    # the limiter again.
    def test_output_volume_only_scales
      full = @engine.render(dnb, 6.0, { pre_amp: 1.92 }).steady
      half = @engine.render(dnb, 6.0, { pre_amp: 1.92, amp: 0.5 }).steady
      n = [full.l.size, half.l.size].min
      worst = 0.0
      (0...n).each do |i|
        d = (half.l[i] - full.l[i] * 0.5).abs
        worst = d if d > worst
        d = (half.r[i] - full.r[i] * 0.5).abs
        worst = d if d > worst
      end
      assert_operator A.db(worst), :<, -120.0,
                      "output volume altered the mix rather than scaling it"
    end

    # Volume is clamped to unity in the synthdef, because a post-limiter gain
    # above 1 would scale the limited signal back over the ceiling and undo
    # the one guarantee the chain makes.
    def test_output_volume_cannot_exceed_unity
      unity = @engine.render(dnb, 6.0, { pre_amp: 2.4, amp: 1.0 }).steady
      over = @engine.render(dnb, 6.0, { pre_amp: 2.4, amp: 4.0 }).steady
      assert_operator A.db(A.sample_peak(over.l)), :<=, 0.0,
                      "amp above unity pushed the output over full scale"
      assert_in_delta A.db(A.sample_peak(unity.l)), A.db(A.sample_peak(over.l)), 0.01,
                      "amp above unity was not clamped to unity"
    end

    # The output must never exceed full scale, at any drive. It is the
    # limiter that guarantees this: it is the last stage in the chain and
    # its ceiling is provably its output bound (see SPLimiterCore.h), so
    # this asserts the real thing rather than a pinned known breach.
    def test_output_respects_the_ceiling
      [1.6, 2.5, 4.0].each do |pre|
        cap = @engine.render(dnb, 6.0, { pre_amp: pre }).steady
        peak = A.db(A.sample_peak(cap.l))
        assert_operator peak, :<=, 0.0, "output peak exceeded full scale at pre_amp #{pre}"
        assert_operator peak, :>, -0.5, "limiter over-reduced at pre_amp #{pre}"
      end
    end

    # Inter-sample peaks are NOT bounded: the limiter detects sample peaks,
    # so a signal sitting exactly on the ceiling can still reconstruct above
    # it between samples. Fixing this needs an oversampled detector per
    # BS.1770-4. Pinned so the cost of not having one stays visible, and so
    # it can be tightened when one is added.
    KNOWN_TRUE_PEAK_OVER_DB = 1.0

    def test_inter_sample_peaks_stay_within_the_known_bound
      cap = @engine.render(dnb, 6.0, { pre_amp: 2.5 }).steady
      assert_operator A.db(A.true_peak(cap.l)), :<, KNOWN_TRUE_PEAK_OVER_DB,
                      "inter-sample peak got worse than the known bound"
    end

    # Driving harder must buy loudness, and it must cost crest factor and
    # gain reduction in a way that increases smoothly. A chain that stops
    # trading one for the other has either stopped limiting or started
    # destroying the signal.
    def test_drive_trades_crest_for_loudness_monotonically
      results = [1.0, 1.6, 2.5, 4.0].map do |pre|
        cap = @engine.render(dnb, 6.0, { pre_amp: pre }).steady
        ref = @engine.render(dnb, 6.0, { pre_amp: pre, limiter_bypass: 1 }).steady
        { lufs: A.lufs_integrated(cap.channels, cap.sample_rate),
          crest: A.crest_factor_db(cap.l),
          gr: A.gain_reduction_db(cap.l, ref.l, cap.sample_rate)[:peak] }
      end
      results.each_cons(2) do |a, b|
        assert_operator b[:lufs], :>, a[:lufs], "loudness must rise with drive"
        assert_operator b[:crest], :<, a[:crest], "crest must fall with drive"
        assert_operator b[:gr], :>=, a[:gr] - 0.1, "gain reduction must not fall"
      end
      assert_operator results.first[:gr], :<, 1.0, "unity drive should barely limit"
    end

    # The limiter's detector is linked across the pair, so both channels are
    # scaled by one identical gain and the stereo image cannot move when it
    # works. What is left is measurement residue, not image shift: at high
    # drive the bypassed reference this is measured against is itself
    # clipped, so out/ref is not purely the applied gain.
    MAX_STEREO_LINK_ERROR_DB = 0.3

    def test_stereo_link_error
      # The extremes: light limiting, and hard enough that the reference
      # itself is clipping.
      [1.6, 4.0].each do |pre|
        cap = @engine.render(dnb, 6.0, { pre_amp: pre }).steady
        ref = @engine.render(dnb, 6.0, { pre_amp: pre, limiter_bypass: 1 }).steady
        error = A.stereo_link_error_db(cap.l, cap.r, ref.l, ref.r, cap.sample_rate)
        assert_operator error, :<, MAX_STEREO_LINK_ERROR_DB,
                        "channels drifted apart at pre_amp #{pre}"
      end
    end

    # Latency is the reason SPLimiter exists: it is added to live input being
    # monitored through the mixer. SC's Limiter delays by twice its dur;
    # SPLimiter must delay by exactly the look-ahead itself. Measured, not
    # assumed: a limiter built against an assumed delay is how a mismatched
    # reference gets compared.
    EXPECTED_LIMITER_LATENCY = 72   # 1.5 ms at 48 kHz

    def test_limiter_latency
      cap = @engine.render(dnb, 6.0, { pre_amp: 2.5 }).steady
      ref = @engine.render(dnb, 6.0, { pre_amp: 2.5, limiter_bypass: 1 }).steady
      assert_equal EXPECTED_LIMITER_LATENCY, A.best_lag(cap.l, ref.l), "left latency"
      assert_equal EXPECTED_LIMITER_LATENCY, A.best_lag(cap.r, ref.r), "right latency"
    end

    # Intermodulation is the measurable form of "mud": drive the limiter hard
    # with two low tones and the products it creates land in the bass. This
    # pins how bad it currently gets so a chain change can be shown to
    # improve it.
    def test_intermodulation_under_drive
      tt = S.write(S.two_tone(duration: 4.0), "test-twotone")
      clean = @engine.render(tt, 4.0, { pre_amp: 1.0 }).steady
      driven = @engine.render(tt, 4.0, { pre_amp: 4.0 }).steady
      assert_operator A.imd_db(clean.l, 220.0, 247.0, clean.sample_rate), :<, -100.0,
                      "must be clean when not limiting"
      assert_operator A.imd_db(driven.l, 220.0, 247.0, driven.sample_rate), :<, -20.0,
                      "IMD under heavy drive got worse than the known bound"
    end
  end
end
