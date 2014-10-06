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

  class SpiderApiTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    def test_rrand_handles_0_range
      assert_equal(1, rrand(1,1))
    end

    def test_rrand_i_handles_0_range
      assert_equal(1, rrand_i(1,1))
    end
  end
end
