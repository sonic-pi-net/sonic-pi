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

  class SpiderApiTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    Thread.current.thread_variable_set(:sonic_pi_spider_random_generator, Random.new(0))

    def test_rrand_handles_0_range
      assert_equal(1, rrand(1,1))
    end

    def test_rrand_i_handles_0_range
      assert_equal(1, rrand_i(1,1))
    end

    def test_rand_handles_0
      number = rand(0)
      assert(number >= 0 && number < 1)
    end

    def test_rand_i_handles_0
      number = rand_i(0)
      assert(number == 0)
    end

    def test_rand_only_returns_floats
      assert_equal(Float, rand(0..10).class)
      assert_equal(Float, rand(1).class)
    end

    def test_rand_i_only_returns_ints
      assert_equal(Fixnum, rand_i(0..1.0).class)
      assert_equal(Fixnum, rand_i(1.5).class)
    end

    def test_quantise
      assert_equal(10.0, quantise(10, 1))
      assert_equal(9.9, quantise(10, 1.1))
      assert_equal(13.3, quantise(13.3212, 0.1))
      assert_equal(13.4, quantise(13.3212, 0.2))
      assert_equal(13.2, quantise(13.3212, 0.3))
      assert_equal(13.5, quantise(13.3212, 0.5))
    end
  end
end
