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

module SonicPi
  # #1497: sample should honour duration: the same way play/synth do - i.e.
  # duration: is the total envelope length and is converted to a concrete
  # sustain:, overriding the sampler's default sustain: -1 ("play the whole
  # buffer"). This gates the sample to duration beats without needing to know
  # the buffer's natural length. When neither duration: nor sustain: is given,
  # sustain stays -1 and the whole sample plays as before.
  class SampleDurationTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    # The sampler's real envelope defaults - pulled from the info so these
    # tests stay honest if the defaults ever change.
    def sample_defaults
      SonicPi::Synths::SynthInfo.get_info(:mono_player).arg_defaults
    end

    # ---------------------------------------------------------------------
    # Value computation: calculate_sustain! against the sampler's own defaults
    # (attack: 0, decay: 0, sustain: -1, release: 0). sustain must come out as
    # duration - (attack + decay + release).
    # ---------------------------------------------------------------------

    def test_duration_alone_becomes_sustain
      args = { duration: 2 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 2, args[:sustain]
      refute args.has_key?(:duration)          # duration consumed
    end

    def test_duration_minus_release
      args = { duration: 2, release: 0.5 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 1.5, args[:sustain]         # 2 - (0 + 0 + 0.5)
    end

    def test_duration_minus_attack_and_release
      args = { duration: 2, attack: 0.2, release: 0.3 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 1.5, args[:sustain]         # 2 - (0.2 + 0 + 0.3)
    end

    def test_duration_minus_decay
      args = { duration: 1, decay: 0.25 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 0.75, args[:sustain]        # 1 - (0 + 0.25 + 0)
    end

    def test_duration_shorter_than_envelope_clamps_to_zero
      args = { duration: 0.2, release: 0.5 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 0, args[:sustain]           # 0.2 - 0.5 => -0.3, clamped
    end

    # ---- the "magic" sustain interactions Sam relies on ----

    def test_no_duration_and_no_sustain_leaves_sustain_untouched
      # This is the whole-sample default: sustain stays -1 (added later by the
      # synthdef defaults), so calculate_sustain! must not invent one.
      args = { release: 0.3 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      refute args.has_key?(:sustain)
    end

    def test_explicit_sustain_wins_over_duration
      # sample :x, sustain: 0, release: 0.3 => percussive. An explicit sustain
      # must always beat duration:, exactly as it does for play.
      args = { duration: 5, sustain: 0, release: 0.3 }
      @lang.send(:calculate_sustain!, args, sample_defaults)
      assert_equal 0, args[:sustain]
    end

    # ---------------------------------------------------------------------
    # Wiring: the real sample arg path (normalise_and_resolve_sample_args) must
    # actually invoke the conversion - this is the bit that was missing. Driven
    # against a real player info at bpm 60 (so beat == second, no scaling skew).
    # ---------------------------------------------------------------------

    def resolve_sample_opts(opts, synth = :mono_player)
      result = nil
      @lang.run do
        use_bpm 60
        info = SonicPi::Synths::SynthInfo.get_info(synth)
        result = send(:normalise_and_resolve_sample_args, "/fake/path.wav", opts, info)
      end
      result
    end

    def test_wiring_duration_is_converted_to_sustain
      res = resolve_sample_opts(duration: 2)
      assert_in_delta 2, res[:sustain], 0.0001
      refute res.has_key?(:duration)
    end

    def test_wiring_duration_minus_release
      res = resolve_sample_opts(duration: 2, release: 0.5)
      assert_in_delta 1.5, res[:sustain], 0.0001
      assert_in_delta 0.5, res[:release], 0.0001
    end

    def test_wiring_no_opts_leaves_whole_sample_default
      # No duration:, no sustain: => no sustain key at all, so the synthdef's
      # -1 (whole sample) default applies. This proves the fix is inert for the
      # common case.
      res = resolve_sample_opts({})
      refute res.has_key?(:sustain)
    end

    def test_wiring_explicit_percussive_sustain_preserved
      res = resolve_sample_opts(sustain: 0, release: 0.3)
      assert_in_delta 0, res[:sustain], 0.0001
      assert_in_delta 0.3, res[:release], 0.0001
    end

    def test_wiring_stereo_player_behaves_the_same
      res = resolve_sample_opts({ duration: 2 }, :stereo_player)
      assert_in_delta 2, res[:sustain], 0.0001
      refute res.has_key?(:duration)
    end

    def test_wiring_unrelated_opt_does_not_add_sustain
      res = resolve_sample_opts(rate: 2)
      refute res.has_key?(:sustain)
    end
  end
end
