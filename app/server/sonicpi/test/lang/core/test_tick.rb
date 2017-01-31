#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
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

  class TickTester < Minitest::Test
    include SonicPi::Lang::Core


    def test_tick_basic_reset
      tick_reset
      assert_equal(0, look)
    end

    def test_tick_set
      tick_reset_all
      tick_set 40
      assert_equal(40, look)
    end

    def test_tick_set_return_val
      tick_reset_all
      assert_equal(40, tick_set(40))
    end

    def test_tick_reset_return_val
      tick_reset_all
      assert_equal(nil, tick_reset)
    end

    def test_tick_reset_all_return_val
      assert_equal(nil, tick_reset_all)
    end

    def test_tick_reset_all
      tick_reset_all
      assert_equal(0, look)
      assert_equal(0, look(:foo))
      assert_equal(0, look(:bar))
      assert_equal(0, look(:baz))

      assert_equal(0, tick)
      assert_equal(0, tick(:foo))
      assert_equal(0, tick(:bar))
      assert_equal(0, tick(:baz))


      assert_equal(1, tick)
      assert_equal(1, tick(:foo))
      assert_equal(1, tick(:bar))
      assert_equal(1, tick(:baz))

      tick_reset_all

      assert_equal(0, look)
      assert_equal(0, look(:foo))
      assert_equal(0, look(:bar))
      assert_equal(0, look(:baz))
    end

    def test_basic_tick_behaviour
      tick_reset_all
      assert_equal(0, tick)
      assert_equal(1, tick)
      assert_equal(2, tick)
      assert_equal(3, tick)
      tick_reset
      assert_equal(0, tick)
    end

    def test_basic_look_behaviour
      tick_reset_all
      assert_equal(0, tick)
      assert_equal(0, look)
      assert_equal(1, tick)
      assert_equal(1, look)
      assert_equal(2, tick)
      assert_equal(2, look)
      assert_equal(3, tick)
      assert_equal(3, look)
      tick_reset
      assert_equal(0, look)
      assert_equal(0, tick)
      assert_equal(0, look)
    end

    def test_keyed_ticks
      tick_reset_all
      assert_equal(0, tick(:foo))
      assert_equal(1, tick(:foo))
      assert_equal(2, tick(:foo))
      assert_equal(0, tick(:bar))
      assert_equal(0, tick(:baz))
      assert_equal(3, tick(:foo))
      assert_equal(1, tick(:bar))
      assert_equal(1, tick(:baz))
      tick_reset # does not affect keyed ticks
      assert_equal(4, tick(:foo))
      assert_equal(2, tick(:bar))
      assert_equal(2, tick(:baz))
      tick_reset_all #affects all ticks of all keys
      assert_equal(0, tick(:foo))
      assert_equal(0, tick(:bar))
      assert_equal(0, tick(:baz))
    end

    def test_string_keyed_ticks
      tick_reset_all
      assert_equal(0, tick("foo"))
      assert_equal(1, tick(:foo))

      assert_equal(1, look("foo"))
      assert_equal(1, look(:foo))
    end

    def test_incremented_ticks
      tick_reset_all

      # adding an increment value is the same as calling tick n times:
      tick(:foo, step: 5)
      5.times do
        tick
      end

      assert_equal(4, look(:foo))
      assert_equal(4, look)

      assert_equal(5, tick(:foo))
      assert_equal(5, look(:foo))

      assert_equal(5, tick)
      assert_equal(5, look)

      tick(:foo, step: 5)
      5.times do
        tick
      end


      assert_equal(10, look(:foo))
      assert_equal(10, look)
    end

    def test_decremented_ticks
      tick_reset_all

      5.times do
        tick
      end

      assert_equal(4, look)

      tick(step: -1)
      assert_equal(3, look)
      assert_equal(4, tick)
      tick(step: -10)
      assert_equal(-6, look)
    end
  end
end
