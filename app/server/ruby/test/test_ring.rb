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

require_relative "./setup_test"
require_relative "../lib/sonicpi/lang/core"

module SonicPi
  class RingTester < Minitest::Test
    include SonicPi::Lang::Core

    def assert_spread(a, b, str)
      bools = str.chars.map {|c| c == 'x'}
      assert_equal(spread(a, b), bools.ring)
    end

    def test_stretch
      assert_equal([:e1, :e1, :e1, :e2, :e2, :e2].ring, stretch([:e1,:e2], 3))
      assert_equal([:a2, :a2, :a3, :a3, :a1, :a1, :a1, :a4, :a4, :a4].ring, stretch([:a2,:a3], 2, [:a1,:a4], 3))

      assert_equal(SonicPi::Core::RingVector, stretch([:e1], 3).class)
      assert_equal([:a2,:a2].ring, stretch(:a2, 2))

      assert_equal(stretch([:e2], 0), ring())

    end

    def test_knit
      assert_equal(knit(:e1, 3), [:e1, :e1, :e1].ring)
      assert_equal(knit(:e1, 3, :c1, 2), [:e1, :e1, :e1, :c1, :c1].ring)
      assert_equal(knit(:e2, -1, :c1, 3), [:c1, :c1, :c1].ring)
      assert_equal(knit(:e1, 3).class, SonicPi::Core::RingVector)
      assert_equal(knit([:e2], 0), ring())
      assert_equal(knit(:e2, -1), ring())

      assert_raises ArgumentError, "even number" do
        assert_equal(knit(:e2, 1, :c3), ring())
      end

    end

    def test_range
      assert_equal([1.0, 2.0, 3.0, 4.0].ring, range(1, 5))
      assert_equal([1.0, 2.0, 3.0, 4.0, 5.0].ring, range(1, 5, inclusive: true))
      assert_equal([1.0, 2.0, 3.0, 4.0].ring, range(1, 5, 1))
      assert_equal([1.0, 2.0, 3.0, 4.0].ring, range(1, 5, step: 1))
      assert_equal(range(1, 5, step: 2), [1.0, 3.0].ring)
      assert_equal(range(1, -5, step: -2), [1.0, -1.0, -3.0].ring)
      assert_equal(range(10, 50, step: 10), [10.0, 20.0, 30.0, 40.0].ring)
      assert_equal(range(1, 5, step: -1), [1.0, 2.0, 3.0, 4.0].ring)
      assert_equal(range(1, 3).class, SonicPi::Core::RingVector)
      assert_equal(range(10, 10, step: -1), ring())

    end

    def test_line
      assert_equal([0.0, 1.0, 2.0, 3.0].ring, line(0, 4, steps: 4))
      assert_equal([0.0, 1.0, 2.0, 3.0, 4.0].ring, line(0, 4, steps: 5, inclusive: true))
      assert_equal([0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5].ring, line(0, 4, steps: 8))
      assert_equal([5.0, 4.0, 3.0, 2.0, 1.0].ring, line(5, 0, steps: 5))
      assert_equal([0.0, 0.5].ring, line(0, 1, steps: 2))
      assert_equal(line(1.0, 3.0).class, SonicPi::Core::RingVector)


      midi_clocks_half = [0.0,
                    0.020833333333333332,
                    0.041666666666666664,
                    0.0625,
                    0.08333333333333333,
                    0.10416666666666666,
                    0.12499999999999999,
                    0.14583333333333331,
                    0.16666666666666666,
                    0.1875,
                    0.20833333333333334,
                    0.22916666666666669,
                    0.25,
                    0.2708333333333333,
                    0.29166666666666663,
                    0.31249999999999994,
                    0.33333333333333326,
                    0.3541666666666666,
                    0.3749999999999999,
                    0.3958333333333332,
                    0.4166666666666665,
                    0.43749999999999983,
                    0.45833333333333315,
        0.47916666666666646].map{|x| x.round(14)}.ring

      assert_equal midi_clocks_half, line(0, 0.5, steps: 24, inclusive: false)
    end

    def test_ring
      assert_equal(ring(1, 2, 3), [1, 2, 3].ring.ring.ring)
      assert_equal(ring(1,2, 3).class, SonicPi::Core::RingVector)
    end

    def test_bools
      assert_equal(bools(1, 0, 1), [true, false, true].ring)

      assert_equal(bools(1, 1, 1), [true, true, true].ring)
      assert_equal(bools(true, false, true), [true, false, true].ring)
      assert_equal(bools(true, nil, true), [true, false, true].ring)
      assert_equal(bools(:a, 1, nil, true, 0), [true, true, false, true, false].ring)
      assert_equal(bools(1,0, 0).class, SonicPi::Core::RingVector)


      assert_equal(bools(), ring())

    end

    def test_spread
      # Tests extracted from https://github.com/samaaron/sonic-pi/issues/1593
      assert_spread(1, 2, "x.")
      assert_spread(1, 3, "x..")
      assert_spread(1, 4, "x...")
      assert_spread(4, 12, "x..x..x..x..")
      assert_spread(2, 3, "x.x")
      assert_spread(2, 5, "x.x..")
      assert_spread(3, 4, "x.xx")
      assert_spread(3, 5, "x.x.x")
      assert_spread(3, 7, "x.x.x..")
      assert_spread(3, 8, "x..x..x.")
      assert_spread(4, 7, "x.x.x.x")
      assert_spread(4, 9, "x.x.x.x..")
      assert_spread(4, 11, "x..x..x..x.")
      assert_spread(5, 6, "x.xxxx")
      assert_spread(5, 7, "x.xx.xx")
      assert_spread(5, 8, "x.xx.xx.")
      assert_spread(5, 9, "x.x.x.x.x")
      assert_spread(5, 11, "x.x.x.x.x..")
      assert_spread(5, 12, "x..x.x..x.x.")
      assert_spread(5, 16, "x..x..x..x..x...") # (obvious misprint in the paper corrected)
      assert_spread(7, 8, "x.xxxxxx")
      assert_spread(7, 12, "x.xx.x.xx.x.")
      assert_spread(7, 16, "x..x.x.x..x.x.x.")
      assert_spread(9, 16, "x.xx.x.x.xx.x.x.")
      assert_spread(11, 24, "x..x.x.x.x.x..x.x.x.x.x.")
      assert_spread(13, 24, "x.xx.x.x.x.x.xx.x.x.x.x.")
    end

    def test_plus
      assert_equal(ring(1, 2, 3) + 10, ring(11.0, 12.0, 13.0))
    end

    def test_list_plus
      assert_equal(ring(1, 2, 3) + ring(4), ring(1, 2, 3, 4))
      assert_equal(ring(1, 2, 3) + ring(1), ring(1, 2, 3, 1))
    end

    def test_minus
      assert_equal(ring(10, 20, 30) - 5, ring(5.0, 15.0, 25.0))
    end

    def test_list_minus
      assert_equal(ring(1, 2, 3) - ring(1), ring(2, 3))
      assert_equal(ring(1, 2, 3) - ring(10), ring(1, 2, 3))
    end

    def test_take
      assert_equal(ring(1), ring(1, 2, 3).take(1))
      assert_equal(ring(1, 2), ring(1, 2, 3).take(2))
      assert_equal(ring(1, 2, 3, 1), ring(1, 2, 3).take(4))
      assert_equal(ring(3), ring(1, 2, 3).take(-1))
      assert_equal(ring(3, 2), ring(1, 2, 3).take(-2))
      assert_equal(ring(3, 2, 1, 3, 2), ring(1, 2, 3).take(-5))
      assert_equal(ring(), ring(1, 2, 3).take(0))
      assert_equal(ring(), ring().take(1))
    end

    def test_reverse
      assert_equal(ring(1, 2, 3), ring(3, 2, 1).reverse)
    end

    def test_scale
      assert_equal(ring(1, 2, 3).scale(2), ring(2.0, 4.0, 6.0))
      assert_equal(ring(1, 2, 3).scale(-2), ring(-2.0, -4.0, -6.0))
      assert_equal(ring(1, 2, 3).scale(0), ring(0.0, 0.0, 0.0))
    end

    def test_ramp
      assert_equal(ramp(1, 2, 3)[100], 3)
      assert_equal(SonicPi::Core::RampVector, ring(1, 2, 3).ramp.class)
      assert_equal(SonicPi::Core::RampVector, ring(1, 2, 3).ramp.ring.class)
    end

    def test_ramp_tick
      tick_reset_all

      r = ramp(1, 2, 3)
      assert_equal(1, r.tick)
      assert_equal(2, r.tick)
      assert_equal(3, r.tick)
      assert_equal(3, r.tick)
    end
  end
end
