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

  class SpiderApiTester < Minitest::Test
    include SonicPi::Lang::Core

    Thread.current.thread_variable_set(:sonic_pi_spider_random_generator, Random.new(0))

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
