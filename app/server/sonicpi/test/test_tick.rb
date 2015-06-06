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

require 'test/unit'
require_relative "../../core"
require_relative "../lib/sonicpi/spiderapi"

module SonicPi

  class TickTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI


    def test_tick_basic_reset
      tick_reset
      assert_equal(0, hook)
    end


    def test_tick_reset_all
      tick_reset_all
      assert_equal(0, hook)
      assert_equal(0, hook(:foo))
      assert_equal(0, hook(:bar))
      assert_equal(0, hook(:baz))

      assert_equal(0, tick)
      assert_equal(0, tick(:foo))
      assert_equal(0, tick(:bar))
      assert_equal(0, tick(:baz))


      assert_equal(1, tick)
      assert_equal(1, tick(:foo))
      assert_equal(1, tick(:bar))
      assert_equal(1, tick(:baz))

      tick_reset_all

      assert_equal(0, hook)
      assert_equal(0, hook(:foo))
      assert_equal(0, hook(:bar))
      assert_equal(0, hook(:baz))
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

    def test_basic_hook_behaviour
      tick_reset_all
      assert_equal(0, tick)
      assert_equal(0, hook)
      assert_equal(1, tick)
      assert_equal(1, hook)
      assert_equal(2, tick)
      assert_equal(2, hook)
      assert_equal(3, tick)
      assert_equal(3, hook)
      tick_reset
      assert_equal(0, hook)
      assert_equal(0, tick)
      assert_equal(0, hook)
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

    def test_incremented_ticks
      tick_reset_all

      # adding an increment value is the same as calling tick n times:
      tick(:foo, 5)
      5.times do
        tick
      end

      assert_equal(4, hook(:foo))
      assert_equal(4, hook)

      assert_equal(5, tick(:foo))
      assert_equal(5, hook(:foo))

      assert_equal(5, tick)
      assert_equal(5, hook)

      tick(:foo, 5)
      5.times do
        tick
      end


      assert_equal(10, hook(:foo))
      assert_equal(10, hook)
    end
  end
end
