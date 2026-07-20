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
  class PlayPatternTimedTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    # ---- calculate_sustain! : duration: honours the synth's default envelope ----
    # Regression for #3514: duration: is documented as the total note length
    # (attack + decay + sustain + release), so converting it to sustain must
    # subtract the synth's *default* envelope, not assume zeros.

    def test_duration_subtracts_default_release
      args = { duration: 1 }
      @lang.send(:calculate_sustain!, args, { release: 1 })
      assert_equal 0, args[:sustain]        # 1 - (0 + 0 + 1)
      refute args.has_key?(:duration)       # duration consumed
    end

    def test_duration_subtracts_all_default_envelope_stages
      args = { duration: 3 }
      @lang.send(:calculate_sustain!, args, { attack: 0.5, decay: 0.5, release: 1 })
      assert_equal 1, args[:sustain]        # 3 - (0.5 + 0.5 + 1)
    end

    def test_explicit_opt_overrides_default
      args = { duration: 2, release: 0 }
      @lang.send(:calculate_sustain!, args, { release: 1 })
      assert_equal 2, args[:sustain]        # 2 - (0 + 0 + 0)
    end

    def test_sustain_clamped_to_zero
      args = { duration: 0.5 }
      @lang.send(:calculate_sustain!, args, { release: 1 })
      assert_equal 0, args[:sustain]        # 0.5 - 1 => -0.5, clamped
    end

    def test_explicit_sustain_left_untouched
      args = { duration: 1, sustain: 5 }
      @lang.send(:calculate_sustain!, args, { release: 1 })
      assert_equal 5, args[:sustain]
      assert_equal 1, args[:duration]       # duration not consumed when sustain given
    end

    # ---- play_pattern_timed : legato default vs envelope-opt total-match ----

    def played_opts(&blk)
      calls = []
      @lang.define_singleton_method(:play) { |note, *a| calls << (a.last.is_a?(Hash) ? a.last : {}) }
      @lang.define_singleton_method(:sleep) { |*| nil }
      @lang.run(&blk)
      calls
    end

    def test_default_stretches_sustain_to_fill_each_slot
      opts = played_opts { play_pattern_timed [40, 42], [1, 2] }
      assert_equal 1, opts[0][:sustain]
      assert_equal 2, opts[1][:sustain]
      refute opts[0].has_key?(:duration)
    end

    def test_release_opt_matches_total_duration
      opts = played_opts { play_pattern_timed [40, 42], [1, 2], release: 0.5 }
      assert_equal 1, opts[0][:duration]
      assert_equal 0.5, opts[0][:release]
      refute opts[0].has_key?(:sustain)
    end

    def test_explicit_sustain_is_honoured
      opts = played_opts { play_pattern_timed [40, 42], [1, 2], sustain: 0 }
      assert_equal 0, opts[0][:sustain]
      refute opts[0].has_key?(:duration)
    end
  end
end
