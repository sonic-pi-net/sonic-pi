#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'test/unit'
require_relative "../lib/sonicpi/spiderapi"

module SonicPi
  class RingTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    def test_knit
      assert_equal(knit(:e1, 3), [:e1, :e1, :e1])
      assert_equal(knit(:e1, 3, :c1, 2), [:e1, :e1, :e1, :c1, :c1])
      assert_equal(knit(:e2, 0), [])
      assert_equal(knit(:e2, -1), [])
      assert_equal(knit(:e2, -1, :c1, 3), [:c1, :c1, :c1])
      assert_equal(knit(:e1, 3).class, SonicPi::Core::RingArray)
    end

    def test_range
      assert_equal(range(1, 5), [1, 2, 3, 4])
      assert_equal(range(1, 5, 1), [1, 2, 3, 4])
      assert_equal(range(1, 5, 2), [1, 3])
      assert_equal(range(1, -5, -2), [1, -1, -3])
      assert_equal(range(10, 50, 10), [10, 20, 30, 40])
      assert_equal(range(1, 5, -1), [1, 2, 3, 4])
      assert_equal(range(10, 10, -1), [])
      assert_equal(range(1, 3).class, SonicPi::Core::RingArray)
    end

    def test_ring
      assert_equal(ring(1, 2, 3), [1, 2, 3])
      assert_equal(ring(1,2, 3).class, SonicPi::Core::RingArray)
    end

    def test_bools
      assert_equal(bools(1, 0, 1), [true, false, true])
      assert_equal(bools(), [])
      assert_equal(bools(1, 1, 1), [true, true, true])
      assert_equal(bools(true, false, true), [true, false, true])
      assert_equal(bools(true, nil, true), [true, false, true])
      assert_equal(bools(:a, 1, nil, true, 0), [true, true, false, true, false])
      assert_equal(bools(1,0, 0).class, SonicPi::Core::RingArray)
    end

    def test_spread
      assert_equal(spread(5, 13), [true, false, false, true, false, false, true, false, true, false, false, true, false])
      assert_equal(spread(3, 8, rotate: 1),  [true, false, false, true, false, true, false, false])
    end
  end
end
