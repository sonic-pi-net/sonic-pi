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

  # Lang::Core defines Sonic Pi's OWN assert/assert_equal (user-facing language
  # fns), which shadow Minitest's if included straight into a Minitest::Test —
  # the test then reports "0 assertions" and its failures surface as errors.
  # Keep the language on a separate harness object so the test class keeps
  # Minitest's assertions.
  class SwingHarness
    include SonicPi::Lang::Core

    attr_reader :warped

    def initialize
      @warped = false
      tick_reset_all
    end

    # with_swing routes the shifted case through time_warp, which needs the
    # full runtime. Stubbing it makes BOTH branches observable, so the tests can
    # pin which one an offset selects rather than only that it didn't raise.
    def time_warp(*_args, &blk)
      @warped = true
      blk.call
    end

    # true when the swing shifted, i.e. (tick + offset) % pulse == 0
    def shifted?(offset, pulse: 2)
      @ran = false
      with_swing(0, pulse: pulse, offset: offset) { @ran = true }
      @warped
    end

    def ran? = @ran
  end

  class WithSwingTester < Minitest::Test

    def shifted(offset, pulse: 2)
      h = SwingHarness.new
      result = h.shifted?(offset, pulse: pulse)
      assert(h.ran?, "the block should always run, shifted or not")
      result
    end

    def test_offset_accepts_whole_float
      assert_equal(shifted(0), shifted(0.0))
    end

    def test_offset_accepts_rational
      assert_equal(shifted(0), shifted(Rational(0, 1)))
    end

    def test_offset_accepts_integer_as_before
      assert_includes([true, false], shifted(0))
    end

    # Rounded to nearest, like the language's other index-like values (sample's
    # slice:/num_slices:, a sample pack's numeric index). 0.6 must select
    # offset 1, not 0 — which is what distinguishes rounding from truncation.
    def test_fractional_offset_rounds_to_nearest
      assert_equal(shifted(1), shifted(0.6), "0.6 should behave as offset 1")
      refute_equal(shifted(0), shifted(0.6),
                   "0.6 must not behave as offset 0 (that would be truncation)")
    end

    def test_non_numeric_offset_still_raises
      assert_raises ArgumentError do
        SwingHarness.new.shifted?(:nope, pulse: 1)
      end
    end

    def test_shift_and_pulse_accept_floats
      h = SwingHarness.new
      ran = false
      h.with_swing(0.0, pulse: 1.0, offset: 0) { ran = true }
      assert(ran, "shift: and pulse: have always accepted floats")
    end
  end
end
