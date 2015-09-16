#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/spiderapi"

module SonicPi
  class RingTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    def test_stretch
      assert_equal([:e1, :e1, :e1, :e2, :e2, :e2], stretch([:e1,:e2], 3))
      assert_equal([:a2, :a2, :a3, :a3, :a1, :a1, :a1, :a4, :a4, :a4], stretch([:a2,:a3], 2, [:a1,:a4], 3))

      assert_equal(SonicPi::Core::RingVector, stretch([:e1], 3).class)
      assert_equal([:a2,:a2], stretch(:a2, 2))

      assert_raise SonicPi::Core::EmptyVectorError do
        stretch([:e2], 0)
      end
    end

    def test_knit
      assert_equal(knit(:e1, 3), [:e1, :e1, :e1])
      assert_equal(knit(:e1, 3, :c1, 2), [:e1, :e1, :e1, :c1, :c1])
      assert_equal(knit(:e2, -1, :c1, 3), [:c1, :c1, :c1])
      assert_equal(knit(:e1, 3).class, SonicPi::Core::RingVector)
      assert_raise SonicPi::Core::EmptyVectorError do
        knit(:e2, 0)
      end

      assert_raise SonicPi::Core::EmptyVectorError do
        knit(:e2, -1)
      end

      assert_raise RuntimeError, "even number" do
        knit(:e2, 1, :c3)
      end

    end

    def test_range
      assert_equal([1, 2, 3, 4], range(1, 5))
      assert_equal([1, 2, 3, 4, 5], range(1, 5, inclusive: true))
      assert_equal([1, 2, 3, 4], range(1, 5, 1))
      assert_equal([1, 2, 3, 4], range(1, 5, step: 1))
      assert_equal(range(1, 5, step: 2), [1, 3])
      assert_equal(range(1, -5, step: -2), [1, -1, -3])
      assert_equal(range(10, 50, step: 10), [10, 20, 30, 40])
      assert_equal(range(1, 5, step: -1), [1, 2, 3, 4])
      assert_equal(range(1, 3).class, SonicPi::Core::RingVector)

      assert_raise SonicPi::Core::EmptyVectorError do
        range(10, 10, step: -1)
      end
    end


    def test_line
      assert_equal([0.0, 1.0, 2.0, 3.0], line(0, 4, steps: 4))
      assert_equal([0.0, 1.0, 2.0, 3.0, 4.0], line(0, 4, steps: 5, inclusive: true))
      assert_equal([0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5], line(0, 4, steps: 8))
      assert_equal([5.0, 4.0, 3.0, 2.0, 1.0], line(5, 0, steps: 5))
      assert_equal([0.0, 0.5], line(0, 1, steps: 2))
      assert_equal(line(1.0, 3.0).class, SonicPi::Core::RingVector)
    end

    def test_ring
      assert_equal(ring(1, 2, 3), [1, 2, 3])
      assert_equal(ring(1,2, 3).class, SonicPi::Core::RingVector)
    end

    def test_bools
      assert_equal(bools(1, 0, 1), [true, false, true])

      assert_equal(bools(1, 1, 1), [true, true, true])
      assert_equal(bools(true, false, true), [true, false, true])
      assert_equal(bools(true, nil, true), [true, false, true])
      assert_equal(bools(:a, 1, nil, true, 0), [true, true, false, true, false])
      assert_equal(bools(1,0, 0).class, SonicPi::Core::RingVector)

      assert_raise SonicPi::Core::EmptyVectorError do
        assert_equal(bools(), [])
      end
    end

    def test_spread
      assert_equal(spread(5, 13), [true, false, false, true, false, false, true, false, true, false, false, true, false])
      assert_equal(spread(3, 8, rotate: 1),  [true, false, false, true, false, true, false, false])
    end
  end
end
