# encoding: utf-8
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
  class SparkTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    def test_spark
      assert_equal("▁▃▅▇", __spark(1, 2, 3, 4))
      assert_equal("▁▃▅▇", __spark([1, 2, 3, 4]))
      assert_equal("▁", __spark(1))
      assert_equal("▁▁▁", __spark(3, 3, 3))
      assert_equal("", __spark([]))
      assert_equal("spark error: can't use nested arrays", __spark([1, 2], 3, 4))
      assert_equal("spark error: arguments should be numeric", __spark('foo', 'apple', 'banana', {silly: :hash}))
    end
  end
end
