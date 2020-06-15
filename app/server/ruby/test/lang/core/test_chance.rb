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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  class ChanceTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_one_in_out_of_bounds
      rand_reset
      500.times do
        assert_equal(false, one_in(0))
        assert_equal(false, one_in(-1))
      end
    end

    def test_pick
      rand_reset
      assert_equal([3], pick([1, 2, 3]))
      rand_reset
      assert_equal([3], pick([1, 2, 3], 1))
      rand_reset
      assert_equal([3, 3], pick([1, 2, 3], 2))
      rand_reset
      assert_equal([3, 3, 2], pick([1, 2, 3], 3))
      rand_reset
      assert_equal([3, 3, 2, 1, 1], pick([1, 2, 3], 5))
      rand_reset
      assert_equal([3, 2, 1, 1], pick([1, 2, 3], 4, skip: 1))
    end

    def test_pick_lambda
      rand_reset
      assert_equal([3], pick.call([1, 2, 3]))
      rand_reset
      assert_equal([3], pick(1).call([1, 2, 3]))
      rand_reset
      assert_equal([3, 3], pick(2).call([1, 2, 3]))
      rand_reset
      assert_equal([3, 3, 2], pick(3).call([1, 2, 3]))
      rand_reset
      assert_equal([3, 3, 2, 1, 1], pick(5).call([1, 2, 3]))
      rand_reset
      assert_equal([3, 2, 1, 1], pick(4, skip: 1).call([1, 2, 3]))
    end

  end
end
